(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Caqti_lwt
open Subsocia_common
open Subsocia_prereq
open Unprime
open Unprime_list
open Unprime_option

let entity_type_grade = 1e-3 *. cache_second
let entity_grade = 1e-3 *. cache_second
let preceq_grade = 1e-2 *. cache_second (* TODO: Highly non-constant. *)
let schema_prefix = ref "subsocia."

module type S = Subsocia_intf.RW

module Q = struct
  open Caqti_query

  let q sql = prepare_fun @@ fun lang ->
    let buf = Buffer.create (String.length sql) in
    let conv =
      match lang with
      | `Pgsql ->
	let n = ref 0 in
	begin function
	| '?' -> n := succ !n; Printf.bprintf buf "$%d" !n
	| '@' -> Buffer.add_string buf !schema_prefix
	| ch -> Buffer.add_char buf ch
	end
      | `Sqlite ->
	begin function
	| '@' -> ()
	| ch -> Buffer.add_char buf ch
	end
      | _ -> raise Missing_query_string in
    String.iter conv sql;
    Buffer.contents buf

  let fetch_entity_type =
    q "SELECT entity_type_id, entity_name FROM @entity_type \
       WHERE entity_type_id = ?"

  let fetch_entity_type_preds =
    q "SELECT subentity_type_id, subentity_multiplicity \
       FROM @inclusion_type WHERE superentity_type_id = ?"
  let fetch_entity_type_succs =
    q "SELECT superentity_type_id, superentity_multiplicity \
       FROM @inclusion_type WHERE subentity_type_id = ?"

  let fetch =
    q "SELECT entity_id, entity_type_id, entity_rank, viewer_id, admin_id \
       FROM @entity WHERE entity_id = ?"

  let select_min_max = [|
    q "SELECT entity_id, entity_type_id, entity_rank, viewer_id, admin_id \
       FROM @entity \
       WHERE not EXISTS \
	(SELECT 0 FROM @inclusion WHERE superentity_id = entity_id)";
    q "SELECT entity_id, entity_type_id, entity_rank, viewer_id, admin_id \
       FROM @entity \
       WHERE not EXISTS \
	(SELECT 0 FROM @inclusion WHERE subentity_id = entity_id)";
  |]

  let select_pred_succ = [|
    q "SELECT entity_id, entity_type_id, entity_rank, viewer_id, admin_id \
       FROM @entity JOIN @inclusion \
	 ON entity_id = subentity_id \
       WHERE superentity_id = ?";
    q "SELECT entity_id, entity_type_id, entity_rank, viewer_id, admin_id \
       FROM @entity JOIN @inclusion \
	 ON entity_id = superentity_id \
       WHERE subentity_id = ?";
  |]

  let select_precedes =
    q (* TODO: Utilize entity_rank when implemented. *)
      "WITH RECURSIVE successors(entity_id) AS ( \
	  SELECT i.superentity_id AS entity_id \
	  FROM @inclusion i \
	  WHERE i.is_subsumed = false \
	    AND i.subentity_id = ? \
	UNION \
	  SELECT i.superentity_id \
	  FROM @inclusion i JOIN successors c \
	    ON i.subentity_id = c.entity_id \
	  WHERE i.is_subsumed = false \
       ) \
       SELECT 0 FROM successors WHERE entity_id = ? LIMIT 1"

  let create_entity =
    q "INSERT INTO @entity (entity_type_id, viewer_id, admin_id) \
       VALUES (?, ?, ?) RETURNING entity_id"

  let insert_inclusion =
    q "INSERT INTO @inclusion (subentity_id, superentity_id) VALUES (?, ?)"

  let delete_inclusion =
    q "DELETE FROM @inclusion WHERE subentity_id = ? AND superentity_id = ?"
end

module Base = struct
  type entity_type_id = int32

  type entity_type = {
    et_id : entity_type_id;
    et_name : string;
    et_preds : (entity_type_id * Multiplicity.t) list;
    et_succs : (entity_type_id * Multiplicity.t) list;
    et_beacon : Beacon.t;
  }

  let dummy_entity_type = {
    et_id = Int32.zero;
    et_name = "";
    et_preds = [];
    et_succs = [];
    et_beacon = Beacon.dummy;
  }

  type entity_id = int32

  type entity = {
    e_id : entity_id;
    e_type_id : entity_type_id;
    e_rank : int;
    e_viewer_id : entity_id;
    e_admin_id : entity_id;
    e_pred_succ : entity_list Weak.t;
    e_beacon : Beacon.t;
  }
  and entity_list = {
    el_entities : entity list;
    el_beacon : Beacon.t;
  }

  let dummy_entity = {
    e_id = Int32.zero;
    e_type_id = Int32.zero;
    e_rank = 0;
    e_viewer_id = Int32.zero;
    e_admin_id = Int32.zero;
    e_pred_succ = Weak.create 2;
    e_beacon = Beacon.dummy;
  }

end

module type AMENDED_CONNECTION = sig
  open Base
  include CONNECTION

  val fetch_entity_type : entity_type_id -> entity_type Lwt.t
  val decode : tuple -> entity
  val fetch : entity_id -> entity Lwt.t
  val min_max : int -> entity list Lwt.t
  val pred_succ : int -> int32 -> entity list Lwt.t
  val precedes : int32 -> int32 -> bool Lwt.t
  val create : entity_type: entity_type -> viewer: entity -> admin: entity ->
	       unit -> entity Lwt.t
  val constrain : entity -> entity -> unit Lwt.t
  val unconstrain : entity -> entity -> unit Lwt.t
end

let connect uri = (module struct

  include Base

  module Entity_type_by_id = Weak.Make (struct
    type t = entity_type
    let equal eA eB = eA.et_id = eB.et_id
    let hash {et_id} = Hashtbl.hash et_id
  end)

  let entity_type_by_id = Entity_type_by_id.create 23

  module Entity_by_id = Weak.Make (struct
    type t = entity
    let equal eA eB = eA.e_id = eB.e_id
    let hash {e_id} = Hashtbl.hash e_id
  end)

  let entity_by_id = Entity_by_id.create 31

  let inclusion_cache = Prime_cache.create ~cache_metric 61

  module Amend_connection (C : CONNECTION) = struct
    include C

    let fetch_entity_type et_id =
      let id_p = C.Param.([|int32 et_id|]) in
      match_lwt C.find Q.fetch_entity_type
		       C.Tuple.(fun tup -> text 1 tup) id_p with
      | None -> Lwt.fail Not_found
      | Some et_name ->
	let decode_rel tup acc =
	  C.Tuple.(int32 0 tup, Multiplicity.of_int (int 1 tup)) :: acc in
	lwt et_preds = C.fold Q.fetch_entity_type_preds decode_rel id_p [] in
	lwt et_succs = C.fold Q.fetch_entity_type_succs decode_rel id_p [] in
	Lwt.return
	  @@ Entity_type_by_id.merge entity_type_by_id
	  @@ Beacon.embed entity_type_grade
	  @@ fun et_beacon -> {et_id; et_name; et_preds; et_succs; et_beacon;}

    let decode tup =
      Entity_by_id.merge entity_by_id @@
      Beacon.embed entity_grade @@ fun e_beacon ->
      { e_id = Tuple.int32 0 tup;
	e_type_id = Tuple.int32 1 tup;
	e_rank = Tuple.int 2 tup;
	e_viewer_id = Tuple.int32 3 tup;
	e_admin_id = Tuple.int32 4 tup;
	e_pred_succ = Weak.create 2;
	e_beacon; }

    let fetch id =
      match_lwt C.find Q.fetch decode C.Param.([|int32 id|]) with
      | None -> Lwt.fail Not_found
      | Some e -> Lwt.return e
    let fetch_viewer e = fetch e.e_viewer_id
    let fetch_admin e = fetch e.e_admin_id

    let min_max i = C.fold Q.select_min_max.(i) (List.push *< decode) [||] []
    let pred_succ i entity_id =
      C.fold Q.select_pred_succ.(i) (List.push *< decode)
	     Param.([|int32 entity_id|]) []

    let precedes subentity_id superentity_id =
      C.find Q.select_precedes (fun _ -> ())
	     Param.([|int32 subentity_id; int32 superentity_id|]) >|=
      function None -> false | Some () -> true

    let create ~entity_type ~viewer ~admin () =
      lwt entity_id_opt =
	C.find Q.create_entity C.Tuple.(int32 0)
	  C.Param.([|int32 entity_type.et_id;
		     int32 viewer.e_id; int32 admin.e_id|]) in
      fetch (Option.get entity_id_opt)

    let constrain subentity superentity =
      C.exec Q.insert_inclusion
	C.Param.([|int32 subentity.e_id; int32 superentity.e_id|])

    let unconstrain subentity superentity =
      C.exec Q.delete_inclusion
	C.Param.([|int32 subentity.e_id; int32 superentity.e_id|])
  end

  let pool =
    let connect () =
      lwt c = Caqti_lwt.connect uri in
      let module C = (val c) in
      let module C' = Amend_connection (C) in
      Lwt.return (module C' : AMENDED_CONNECTION) in
    let disconnect (module C : AMENDED_CONNECTION) = C.disconnect () in
    let validate (module C : AMENDED_CONNECTION) = C.validate () in
    let check (module C : AMENDED_CONNECTION) = C.check in
    Caqti_lwt.Pool.create ~validate ~check connect disconnect

  let use f = Caqti_lwt.Pool.use f pool

  module Entity_type = struct
    type id = int32
    type t = entity_type

    let fetch et_id =
      try
	let et = {dummy_entity_type with et_id} in
	Lwt.return (Entity_type_by_id.find entity_type_by_id et)
      with Not_found ->
	use (fun (module C) -> C.fetch_entity_type et_id)

    let get_id {et_id} = et_id
    let get_name {et_name} = et_name
    let get_preds {et_preds} = et_preds
    let get_succs {et_succs} = et_succs
  end

  module Entity = struct

    type t = entity
    type id = entity_id

    let get_id {e_id} = e_id
    let get_type_id {e_type_id} = e_type_id
    let get_viewer_id {e_viewer_id} = e_viewer_id
    let get_admin_id {e_admin_id} = e_admin_id

    let fetch e_id =
      try Lwt.return (Entity_by_id.find entity_by_id {dummy_entity with e_id})
      with Not_found -> use (fun (module C) -> C.fetch e_id)

    let fetch_type {e_type_id} = Entity_type.fetch e_type_id
    let fetch_admin {e_admin_id} = fetch e_admin_id
    let fetch_viewer {e_viewer_id} = fetch e_viewer_id

    let min_max_cache = Weak.create 2

    let fetch_min_max i =
      match Weak.get min_max_cache i with
      | Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
      | None ->
	lwt el_entities = use (fun (module C) -> C.min_max i) in
	begin match Weak.get min_max_cache i with
	| Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
	| None ->
	  let mk el_beacon = {el_entities; el_beacon} in
	  let el = Beacon.embed entity_grade mk in
	  Weak.set min_max_cache i (Some el); Lwt.return el.el_entities
	end

    let fetch_min () = fetch_min_max 0
    let fetch_max () = fetch_min_max 1

    let fetch_pred_succ i e =
      match Weak.get e.e_pred_succ i with
      | Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
      | None ->
	lwt el_entities = use (fun (module C) -> C.pred_succ i e.e_id) in
	begin match Weak.get e.e_pred_succ i with
	| Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
	| None ->
	  let mk el_beacon = {el_entities; el_beacon} in
	  let el = Beacon.embed entity_grade mk in
	  Weak.set e.e_pred_succ i (Some el); Lwt.return el.el_entities
	end

    let fetch_preds = fetch_pred_succ 0
    let fetch_succs = fetch_pred_succ 1

    let create ~entity_type ~viewer ~admin () =
      use (fun (module C) -> C.create ~entity_type ~viewer ~admin ())

    let check_preceq subentity superentity =
      if subentity.e_id = superentity.e_id then Lwt.return_true else
      let k = subentity.e_id, superentity.e_id in
      try Lwt.return (Prime_cache.find inclusion_cache k)
      with Not_found ->
	lwt c = use (fun (module C) ->
		     C.precedes subentity.e_id superentity.e_id) in
	Prime_cache.replace inclusion_cache preceq_grade k c;
	Lwt.return c

    let constrain subentity superentity =
      lwt is_sub = check_preceq subentity superentity in
      if is_sub then Lwt.return_unit else
      lwt is_super = check_preceq superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      (* TODO: Update entity_rank. *)
      use (fun (module C) -> C.constrain subentity superentity)
      (* TODO: Update is_subsumed. *)
      (* FIXME: Clear or update pred and succ caches. *)
      (* FIXME: Clear or update inclusion cache. *)

    let unconstrain subentity superentity =
      (* TODO: Update is_subsumed. *)
      use (fun (module C) -> C.unconstrain subentity superentity)
      (* TODO: Update entity_rank. *)
      (* FIXME: Clear or update pred and succ caches. *)
      (* FIXME: Clear or update inclusion cache. *)
  end

end : S)
