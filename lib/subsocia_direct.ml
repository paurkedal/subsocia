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

let entity_plugins = Hashtbl.create 11
let register_entity_plugin name plugin = Hashtbl.add entity_plugins name plugin

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
    q "SELECT entity_type_id, entity_type_name, entity_plugin FROM @entity_type \
       WHERE entity_type_id = ?"

  let fetch_entity_type_by_lang =
    q "SELECT lang, display_name, display_name_pl FROM @entity_type_by_lang \
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
    et_plugin_name : string;
    et_plugin : (module Subsocia_intf.ENTITY_PLUGIN) Lazy.t;
    et_display_name : Twine.t;
    et_display_name_pl : Twine.t;
    et_preds : (entity_type_id * Multiplicity.t) list;
    et_succs : (entity_type_id * Multiplicity.t) list;
    et_beacon : Beacon.t;
  }

  let dummy_entity_type = {
    et_id = Int32.zero;
    et_name = "";
    et_plugin_name = "";
    et_plugin = lazy (assert false);
    et_display_name = Twine.make [];
    et_display_name_pl = Twine.make [];
    et_preds = [];
    et_succs = [];
    et_beacon = Beacon.dummy;
  }

  type entity_id = int32

  type entity_attr = {
    ea_id : entity_id;
    ea_fetch_display_name : langs: lang list -> string Lwt.t;
    ea_fetch_attribute : 'a. 'a attribute_key -> 'a Lwt.t;
    ea_store_attribute : 'a. 'a attribute_key -> 'a -> unit Lwt.t;
    ea_beacon : Beacon.t;
  }

  let dummy_entity_attr = {
    ea_id = Int32.zero;
    ea_fetch_display_name = (fun ~langs -> assert false);
    ea_fetch_attribute = (fun key -> assert false);
    ea_store_attribute = (fun key v -> assert false);
    ea_beacon = Beacon.dummy;
  }

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

let connect uri = (module struct

  include Base

  module Entity_type_by_id = Weak.Make (struct
    type t = entity_type
    let equal eA eB = eA.et_id = eB.et_id
    let hash {et_id} = Hashtbl.hash et_id
  end)
  let entity_type_by_id = Entity_type_by_id.create 23

  module Entity_attr_by_id = Weak.Make (struct
    type t = entity_attr
    let equal eaA eaB = eaA.ea_id = eaB.ea_id
    let hash {ea_id} = Hashtbl.hash ea_id
  end)
  let entity_attr_by_id = Entity_attr_by_id.create 23

  module Entity_by_id = Weak.Make (struct
    type t = entity
    let equal eA eB = eA.e_id = eB.e_id
    let hash {e_id} = Hashtbl.hash e_id
  end)
  let entity_by_id = Entity_by_id.create 31

  let inclusion_cache = Prime_cache.create ~cache_metric 61

  let pool =
    let connect () = Caqti_lwt.connect uri in
    let disconnect (module C : CONNECTION) = C.disconnect () in
    let validate (module C : CONNECTION) = C.validate () in
    let check (module C : CONNECTION) = C.check in
    Caqti_lwt.Pool.create ~validate ~check connect disconnect

  let use f = Caqti_lwt.Pool.use f pool

  module Entity_type = struct
    type id = int32
    type t = entity_type

    let fetch' et_id (module C : CONNECTION) =
      let id_p = C.Param.([|int32 et_id|]) in
      match_lwt C.find Q.fetch_entity_type
		       C.Tuple.(fun tup -> text 1 tup, text 2 tup) id_p with
      | None -> Lwt.fail Not_found
      | Some (et_name, et_plugin_name) ->
	lwt display_name, display_name_pl =
	  let add tup (acc, acc_pl) =
	    let open C.Tuple in
	    let lang = int 0 tup in
	    ((lang, text 1 tup) :: acc,
	     (match option text 2 tup
	      with None -> acc_pl | Some s -> (lang, s) :: acc_pl)) in
	  C.fold Q.fetch_entity_type_by_lang add id_p ([], []) in
	let et_display_name = Twine.make display_name in
	let et_display_name_pl = Twine.make display_name_pl in
	let decode_rel tup acc =
	  C.Tuple.(int32 0 tup, Multiplicity.of_int (int 1 tup)) :: acc in
	lwt et_preds = C.fold Q.fetch_entity_type_preds decode_rel id_p [] in
	lwt et_succs = C.fold Q.fetch_entity_type_succs decode_rel id_p [] in
	let et_plugin = lazy
	  try Hashtbl.find entity_plugins et_plugin_name
	  with Not_found ->
	    failwith ("Missing plugin " ^ et_plugin_name ^ ".") in
	Lwt.return
	  @@ Entity_type_by_id.merge entity_type_by_id
	  @@ Beacon.embed entity_type_grade
	  @@ fun et_beacon ->
	  { et_id; et_plugin_name; et_plugin;
	    et_name; et_display_name; et_display_name_pl;
	    et_preds; et_succs; et_beacon }

    let fetch et_id =
      try
	let et = {dummy_entity_type with et_id} in
	Lwt.return (Entity_type_by_id.find entity_type_by_id et)
      with Not_found ->
	use (fetch' et_id)

    let get_id {et_id} = et_id
    let get_name {et_name} = et_name
    let get_display_name ~langs ?(pl = false) et =
      if pl then begin
	try
	  try Twine.to_string ~langs et.et_display_name_pl
	  with Not_found -> Twine.to_string ~langs et.et_display_name
	with Not_found ->
	  et.et_name ^ " entities"
      end else begin
	try
	  Twine.to_string ~langs et.et_display_name
	with Not_found ->
	  et.et_name ^ " entity"
      end
    let get_preds {et_preds} = et_preds
    let get_succs {et_succs} = et_succs

    let get_plugin {et_plugin} = Lazy.force et_plugin
  end

  module Entity = struct

    type t = entity
    type id = entity_id

    let get_id {e_id} = e_id
    let get_type_id {e_type_id} = e_type_id
    let get_viewer_id {e_viewer_id} = e_viewer_id
    let get_admin_id {e_admin_id} = e_admin_id

    let decode (type tuple) (module C : CONNECTION with type Tuple.t = tuple)
	       (tup : tuple) =
      Entity_by_id.merge entity_by_id @@
      Beacon.embed entity_grade @@ fun e_beacon ->
      { e_id = C.Tuple.int32 0 tup;
	e_type_id = C.Tuple.int32 1 tup;
	e_rank = C.Tuple.int 2 tup;
	e_viewer_id = C.Tuple.int32 3 tup;
	e_admin_id = C.Tuple.int32 4 tup;
	e_pred_succ = Weak.create 2;
	e_beacon; }

    let fetch e_id =
      try Lwt.return (Entity_by_id.find entity_by_id {dummy_entity with e_id})
      with Not_found ->
	use @@ fun (module C) ->
	match_lwt C.find Q.fetch (decode (module C))
			 C.Param.([|int32 e_id|]) with
	| None -> Lwt.fail Not_found
	| Some e -> Lwt.return e

    let fetch_type e = Entity_type.fetch e.e_type_id
    let fetch_viewer e = fetch e.e_viewer_id
    let fetch_admin e = fetch e.e_admin_id

    let min_max_cache = Weak.create 2

    let fetch_min_max i =
      match Weak.get min_max_cache i with
      | Some el -> Beacon.charge el.el_beacon; Lwt.return el.el_entities
      | None ->
	lwt el_entities = use @@ fun (module C) ->
	  C.fold Q.select_min_max.(i)
		 (List.push *< decode (module C)) [||] [] in
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
	lwt el_entities = use @@ fun (module C) ->
	  C.fold Q.select_pred_succ.(i) (List.push *< decode (module C))
		 C.Param.([|int32 e.e_id|]) [] in
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
      use @@ fun (module C) ->
	lwt entity_id_opt =
	  C.find Q.create_entity C.Tuple.(int32 0)
	    C.Param.([|int32 entity_type.et_id;
		       int32 viewer.e_id; int32 admin.e_id|]) in
	fetch (Option.get entity_id_opt)

    let check_preceq subentity superentity =
      if subentity.e_id = superentity.e_id then Lwt.return_true else
      let k = subentity.e_id, superentity.e_id in
      try Lwt.return (Prime_cache.find inclusion_cache k)
      with Not_found ->
	lwt c = use @@ fun (module C) ->
	  C.find Q.select_precedes (fun _ -> ())
		 C.Param.([|int32 subentity.e_id; int32 superentity.e_id|]) >|=
	  function None -> false | Some () -> true in
	Prime_cache.replace inclusion_cache preceq_grade k c;
	Lwt.return c

    let constrain' subentity superentity (module C : CONNECTION) =
      C.exec Q.insert_inclusion
	C.Param.([|int32 subentity.e_id; int32 superentity.e_id|])

    let unconstrain' subentity superentity (module C : CONNECTION) =
      C.exec Q.delete_inclusion
	C.Param.([|int32 subentity.e_id; int32 superentity.e_id|])

    let constrain subentity superentity =
      lwt is_sub = check_preceq subentity superentity in
      if is_sub then Lwt.return_unit else
      lwt is_super = check_preceq superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      (* TODO: Update entity_rank. *)
      use (constrain' subentity superentity)
      (* TODO: Update is_subsumed. *)
      (* FIXME: Clear or update pred and succ caches. *)
      (* FIXME: Clear or update inclusion cache. *)

    let unconstrain subentity superentity =
      (* TODO: Update is_subsumed. *)
      use (unconstrain' subentity superentity)
      (* TODO: Update entity_rank. *)
      (* FIXME: Clear or update pred and succ caches. *)
      (* FIXME: Clear or update inclusion cache. *)

    let fetch_plugin e = fetch_type e >|= Entity_type.get_plugin

    let fetch_attr e =
      try Lwt.return (Entity_attr_by_id.find entity_attr_by_id
			{dummy_entity_attr with ea_id = e.e_id})
      with Not_found ->
	lwt ep = fetch_plugin e in
	let module P = (val ep : Subsocia_intf.ENTITY_PLUGIN) in
	lwt ea = P.fetch e.e_id in
	let ea_fetch_attribute k = P.fetch_attribute ea k in
	let ea_store_attribute k v = P.store_attribute ea k v in
	let ea = Beacon.embed entity_grade @@ fun ea_beacon ->
	  { ea_id = e.e_id;
	    ea_fetch_display_name = P.fetch_display_name ea;
	    ea_fetch_attribute; ea_store_attribute;
	    ea_beacon; } in
	Lwt.return ea

    let fetch_display_name e ~langs =
      fetch_attr e >>= fun ea -> ea.ea_fetch_display_name ~langs
    let fetch_attribute e k =
      fetch_attr e >>= fun ea -> ea.ea_fetch_attribute k
    let store_attribute e k v =
      fetch_attr e >>= fun ea -> ea.ea_store_attribute k v
  end

end : S)
