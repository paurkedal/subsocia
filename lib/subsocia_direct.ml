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
open Subsocia_prereq
open Unprime
open Unprime_list
open Unprime_option

let entity_grade = 1e-3 *. cache_second

module type S = Subsocia_intf.RW

module Q = struct
  open Caqti_query

  let fetch = prepare_fun @@ function
    | `Pgsql ->
      "SELECT entity_id, entity_type, entity_rank, \
	      entity_viewer_id, entity_admin_id \
       FROM subsocia.entity WHERE entity_id = $1"
    | _ -> raise Missing_query_string

  let select_min_max = [|
    prepare_sql
      "SELECT entity_id, entity_type, entity_rank, \
	      entity_viewer_id, entity_admin_id \
       FROM subsocia.entity \
       WHERE not EXISTS \
	(SELECT 0 FROM subsocia.inclusion WHERE superentity_id = entity_id)";
    prepare_sql
      "SELECT entity_id, entity_type, entity_rank, \
	      entity_viewer_id, entity_admin_id \
       FROM subsocia.entity \
       WHERE not EXISTS \
	(SELECT 0 FROM subsocia.inclusion WHERE subentity_id = entity_id)";
  |]

  let select_pred_succ = Array.map prepare_fun [|
    begin function
    | `Pgsql ->
      "SELECT entity_id, entity_type, entity_rank, \
	      entity_viewer_id, entity_admin_id \
       FROM subsocia.entity JOIN subsocia.inclusion \
	 ON entity_id = subentity_id \
       WHERE superentity_id = $1"
    | _ -> raise Missing_query_string
    end;
    begin function
    | `Pgsql ->
      "SELECT entity_id, entity_type, entity_rank, \
	      entity_viewer_id, entity_admin_id \
       FROM subsocia.entity JOIN subsocia.inclusion \
	 ON entity_id = superentity_id \
       WHERE subentity_id = $1"
    | _ -> raise Missing_query_string
    end;
  |]

  let create_entity = prepare_fun @@ function
    | `Pgsql ->
      "INSERT INTO subsocia.entity \
	(entity_type, entity_viewer_id, entity_admin_id) \
       VALUES ($1, $2, $3) RETURNING entity_id"
    | _ -> raise Missing_query_string

  let insert_inclusion = prepare_fun @@ function
    | `Pgsql ->
      "INSERT INTO subsocia.inclusion (subentity_id, superentity_id) \
       VALUES ($1, $2)"
    | _ -> raise Missing_query_string

  let delete_inclusion = prepare_fun @@ function
    | `Pgsql ->
      "DELETE FROM subsocia.inclusion \
       WHERE subentity_id = $1 AND superentity_id = $2"
    | _ -> raise Missing_query_string
end

module Base = struct

  type entity_id = int32
  type entity_type = int

  type entity =
    { entity_id : entity_id;
      entity_type : entity_type;
      entity_rank : int;
      entity_viewer_id : entity_id;
      entity_admin_id : entity_id;
      entity_pred_succ : entity_list Weak.t;
      entity_beacon : Beacon.t; }
  and entity_list =
    { el_entities : entity list;
      el_beacon : Beacon.t; }

  let dummy_entity =
    { entity_id = Int32.zero;
      entity_type = 0;
      entity_rank = 0;
      entity_viewer_id = Int32.zero;
      entity_admin_id = Int32.zero;
      entity_pred_succ = Weak.create 2;
      entity_beacon = Beacon.dummy; }

  let entity_id {entity_id} = entity_id
  let entity_type {entity_type} = entity_type
  let entity_viewer_id {entity_viewer_id} = entity_viewer_id
  let entity_admin_id {entity_admin_id} = entity_admin_id

end

module type AMENDED_CONNECTION = sig
  open Base
  include CONNECTION

  val decode : tuple -> entity
  val fetch : entity_id -> entity Lwt.t
  val min_max : int -> entity list Lwt.t
  val pred_succ : int -> int32 -> entity list Lwt.t
  val create : entity_type: int -> viewer: entity -> admin: entity ->
	       unit -> entity Lwt.t
  val constrain : entity -> entity -> unit Lwt.t
  val unconstrain : entity -> entity -> unit Lwt.t
end

let connect uri = (module struct

  include Base

  module Entity_by_id = Weak.Make (struct
    type t = entity
    let equal eA eB = eA.entity_id = eB.entity_id
    let hash {entity_id} = Hashtbl.hash entity_id
  end)

  let entity_by_id = Entity_by_id.create 31

  module Amend_connection (C : CONNECTION) = struct
    include C

    let decode tup =
      Entity_by_id.merge entity_by_id @@
      Beacon.embed entity_grade @@ fun entity_beacon ->
      { entity_id = Tuple.int32 0 tup;
	entity_type = Tuple.int 1 tup;
	entity_rank = Tuple.int 2 tup;
	entity_viewer_id = Tuple.int32 3 tup;
	entity_admin_id = Tuple.int32 4 tup;
	entity_pred_succ = Weak.create 2;
	entity_beacon; }

    let fetch id =
      match_lwt C.find Q.fetch decode C.Param.([|int32 id|]) with
      | None -> Lwt.fail Not_found
      | Some e -> Lwt.return e
    let entity_viewer e = fetch e.entity_viewer_id
    let entity_admin e = fetch e.entity_admin_id

    let min_max i = C.fold Q.select_min_max.(i) (List.push *< decode) [||] []
    let pred_succ i entity_id =
      C.fold Q.select_pred_succ.(i) (List.push *< decode)
	     Param.([|int32 entity_id|]) []

    let create ~entity_type ~viewer ~admin () =
      lwt entity_id_opt =
	C.find Q.create_entity C.Tuple.(int32 0)
	  C.Param.([|int entity_type;
		     int32 viewer.entity_id; int32 admin.entity_id|]) in
      fetch (Option.get entity_id_opt)

    let constrain subentity superentity =
      C.exec Q.insert_inclusion
	C.Param.([|int32 subentity.entity_id; int32 superentity.entity_id|])

    let unconstrain subentity superentity =
      C.exec Q.delete_inclusion
	C.Param.([|int32 subentity.entity_id; int32 superentity.entity_id|])
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

  let fetch entity_id =
    try
      Lwt.return (Entity_by_id.find entity_by_id {dummy_entity with entity_id})
    with Not_found ->
      use (fun (module C) -> C.fetch entity_id)

  let entity_admin {entity_admin_id} = fetch entity_admin_id
  let entity_viewer {entity_viewer_id} = fetch entity_viewer_id

  let min_max_cache = Weak.create 2

  let min_max i =
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

  let min () = min_max 0
  let max () = min_max 1

  let pred_succ i e =
    match Weak.get e.entity_pred_succ i with
    | Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
    | None ->
      lwt el_entities = use (fun (module C) -> C.pred_succ i e.entity_id) in
      begin match Weak.get e.entity_pred_succ i with
      | Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
      | None ->
	let mk el_beacon = {el_entities; el_beacon} in
	let el = Beacon.embed entity_grade mk in
	Weak.set e.entity_pred_succ i (Some el); Lwt.return el.el_entities
      end

  let pred = pred_succ 0
  let succ = pred_succ 1

  let create ~entity_type ~viewer ~admin () =
    use (fun (module C) -> C.create ~entity_type ~viewer ~admin ())

  let preceq subentity superentity = assert false (* FIXME *)

  let constrain subentity superentity =
    lwt is_sub = preceq subentity superentity in
    if is_sub then Lwt.return_unit else
    lwt is_super = preceq superentity subentity in
    if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
    use (fun (module C) -> C.constrain subentity superentity)
    (* FIXME: Clear or update pred and succ caches. *)
    (* FIXME: Clear or update preceq cache. *)

  let unconstrain subentity superentity =
    use (fun (module C) -> C.unconstrain subentity superentity)
    (* FIXME: Clear or update pred and succ caches. *)
    (* FIXME: Clear or update preceq cache. *)

end : S)
