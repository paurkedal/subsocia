(* Copyright (C) 2014--2015  Petter Urkedal <paurkedal@gmail.com>
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
open Panograph_i18n
open Subsocia_common
open Subsocia_prereq
open Unprime
open Unprime_list
open Unprime_option

let fetch_grade = 1e-3 *. cache_second
let attribute_key_grade = fetch_grade
let entity_type_grade = fetch_grade
let entity_grade = fetch_grade
let preceq_grade = 1e-2 *. cache_second (* TODO: Highly non-constant. *)
let attribution_grade = fetch_grade

let schema_prefix = ref "subsocia."

let format_query sql = Caqti_query.prepare_fun @@ fun lang ->
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
    | _ -> raise Caqti_query.Missing_query_string in
  String.iter conv sql;
  Buffer.contents buf

module type CONNECTION_POOL = sig
  val pool : (module Caqti_lwt.CONNECTION) Caqti_lwt.Pool.t
end

module type S = Subsocia_intf.S

module Q = struct
  open Caqti_query

  let q = format_query

  (* Attribute key *)

  let attribute_key_by_id =
    q "SELECT attribute_name, value_type FROM @attribute_key \
       WHERE attribute_key_id = ?"
  let attribute_key_by_name =
    q "SELECT attribute_key_id, value_type FROM @attribute_key \
       WHERE attribute_name = ?"

  (* Entity types *)

  let entity_type_id_of_name =
    q "SELECT entity_type_id FROM @entity_type WHERE entity_type_name = ?"
  let entity_type_name_of_id =
    q "SELECT entity_type_name FROM @entity_type WHERE entity_type_id = ?"

  let inclusion_type_preds =
    q "SELECT subentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type WHERE superentity_type_id = ?"
  let inclusion_type_succs =
    q "SELECT superentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type WHERE subentity_type_id = ?"

  let attribution_type =
    q "SELECT attribute_key_id, attribute_multiplicity \
       FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"
(*
  let attribution_type_preds =
    q "SELECT subentity_type_id, attribution_key_id, attribute_multiplicity \
       FROM @attribution_type WHERE superentity_type_id = ?"
  let attribution_type_succs =
    q "SELECT superentity_type_id, attribution_type_id, attribute_multiplicity \
       FROM @attribution_type WHERE subentity_type_id = ?"
*)

  (* Entites *)

  let inclusion_succs =
    q "SELECT superentity_id FROM @inclusion WHERE subentity_id = ?"
  let inclusion_preds =
    q "SELECT subentity_id FROM @inclusion WHERE superentity_id = ?"

  let entity_type = q "SELECT entity_type_id FROM @entity WHERE entity_id = ?"
  let entity_viewer = q "SELECT viewer_id FROM @entity WHERE entity_id = ?"
  let entity_admin = q "SELECT admin_id FROM @entity WHERE entity_id = ?"

  let minimums =
    q "SELECT entity_id FROM @entity \
       WHERE not EXISTS \
	(SELECT 0 FROM @inclusion WHERE superentity_id = entity_id)"

  let maximums =
    q "SELECT entity_id FROM @entity \
       WHERE not EXISTS \
	(SELECT 0 FROM @inclusion WHERE subentity_id = entity_id)"

  let entity_preds =
    q "SELECT entity_id FROM @entity JOIN @inclusion \
			  ON entity_id = subentity_id \
       WHERE superentity_id = ?"

  let entity_succs =
    q "SELECT entity_id FROM @entity JOIN @inclusion \
			  ON entity_id = superentity_id \
       WHERE subentity_id = ?"

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

  let select_text_attribution =
    q "SELECT value FROM @text_attribution \
       WHERE subentity_id = ? AND superentity_id = ? AND attribute_key_id = ?"
  let select_integer_attribution =
    q "SELECT value FROM @integer_attribution \
       WHERE subentity_id = ? AND superentity_id = ? AND attribute_key_id = ?"
end

let memo_1lwt f =
  let cache = Prime_cache.create ~cache_metric 23 in
  let g x =
    try Lwt.return (Prime_cache.find cache x)
    with Not_found ->
      lwt y = f x in
      Prime_cache.replace cache fetch_grade x y;
      Lwt.return y in
  g, cache

let memo_0lwt = memo_1lwt
let memo_2lwt f = let g, c = memo_1lwt f in (fun x0 x1 -> g (x0, x1)), c
let memo_3lwt f = let g, c = memo_1lwt f in (fun x0 x1 x2 -> g (x0, x1, x2)), c

module Base = struct
  type attribute_key_id = int32
  type entity_type_id = int32
  type entity_id = int32

  module Id_set = Prime_enumset.Make (Int32)
  module Id_map = Prime_enummap.Make (Int32)

  module Attribute_key_base = struct
    type 'a t1 = {
      ak_id : attribute_key_id;
      ak_name : string;
      ak_value_type : 'a Type.t1;
      ak_beacon : Beacon.t;
    }
    type t0 = Ex : 'a t1 -> t0

    let dummy = {
      ak_id = Int32.zero;
      ak_name = "";
      ak_value_type = Type.Bool;
      ak_beacon = Beacon.dummy;
    }

    let type0 (Ex ak) = Type.Ex ak.ak_value_type
    let type1 ak = ak.ak_value_type
  end
end

let connect uri = (module struct

  include Base

  module Attribute_key_by_id = Weak.Make (struct
    open Attribute_key_base
    type t = t0
    let equal (Ex akA) (Ex akB) = akA.ak_id = akB.ak_id
    let hash (Ex {ak_id}) = Hashtbl.hash ak_id
  end)
  let attribute_key_by_id = Attribute_key_by_id.create 23

  module Attribute_key_by_name = Weak.Make (struct
    open Attribute_key_base
    type t = t0
    let equal (Ex akA) (Ex akB) = akA.ak_name = akB.ak_name
    let hash (Ex {ak_name}) = Hashtbl.hash ak_name
  end)
  let attribute_key_by_name = Attribute_key_by_name.create 23

  let inclusion_cache = Prime_cache.create ~cache_metric 61

  let bool_attribute_cache :
    (entity_id * entity_id * Attribute_key_base.t0, bool list) Prime_cache.t =
    Prime_cache.create ~cache_metric 61
  let int_attribute_cache :
    (entity_id * entity_id * Attribute_key_base.t0, int list) Prime_cache.t =
    Prime_cache.create ~cache_metric 61
  let string_attribute_cache :
    (entity_id * entity_id * Attribute_key_base.t0, string list) Prime_cache.t =
    Prime_cache.create ~cache_metric 61

  let pool =
    let connect () = Caqti_lwt.connect uri in
    let disconnect (module C : CONNECTION) = C.disconnect () in
    let validate (module C : CONNECTION) = C.validate () in
    let check (module C : CONNECTION) = C.check in
    Caqti_lwt.Pool.create ~validate ~check connect disconnect

  let with_db f = Caqti_lwt.Pool.use f pool

  module Attribute_key = struct
    include Attribute_key_base

    module Comparable = struct
      type t = t0
      let compare (Ex x) (Ex y) = compare x.ak_id y.ak_id
    end
    module Map = Prime_enummap.Make (Comparable)
    module Set = Prime_enumset.Make (Comparable)

    let id (Ex ak) = ak.ak_id
    let name (Ex ak) = Lwt.return ak.ak_name

    let of_id ak_id =
      try let ak = Ex {dummy with ak_id} in
	  Lwt.return (Attribute_key_by_id.find attribute_key_by_id ak)
      with Not_found ->
	with_db @@ fun (module C : CONNECTION) ->
	lwt ak_name, value_type =
	  C.find Q.attribute_key_by_id
		 C.Tuple.(fun tup -> text 0 tup, text 1 tup)
		 C.Param.([|int32 ak_id|]) in
	match Type.of_string value_type with
	| Type.Ex ak_value_type ->
	  Lwt.return @@ Beacon.embed attribute_key_grade @@ fun ak_beacon ->
	    (Ex {ak_id; ak_name; ak_value_type; ak_beacon})

    let of_name ak_name =
      try let ak = Ex {dummy with ak_name} in
	  Lwt.return (Attribute_key_by_name.find attribute_key_by_name ak)
      with Not_found ->
	with_db @@ fun (module C : CONNECTION) ->
	match_lwt
	  C.find_opt Q.attribute_key_by_name
		     C.Tuple.(fun tup -> int32 0 tup, text 1 tup)
		     C.Param.([|text ak_name|])
	with
	| None -> Lwt.fail Not_found
	| Some (ak_id, value_type) ->
	  match Type.of_string value_type with
	  | Type.Ex ak_value_type ->
	    Lwt.return @@ Beacon.embed attribute_key_grade @@ fun ak_beacon ->
	      (Ex {ak_id; ak_name; ak_value_type; ak_beacon})
  end

  module Entity_type = struct
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    let compare = Int32.compare

    let of_id et = Lwt.return et
    let id et = et

    let of_name, of_name_cache =
      memo_1lwt @@ fun name ->
      with_db @@ fun (module C) ->
      C.find_opt Q.entity_type_id_of_name
		 C.Tuple.(int32 0) C.Param.([|text name|])

    let name, name_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      C.find Q.entity_type_name_of_id C.Tuple.(text 0) C.Param.([|int32 et|])

    let inclusion_preds, inclusion_preds_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.fold Q.inclusion_type_preds
	     C.Tuple.(fun tup -> Map.add (int32 0 tup) (mult 1 tup, mult 2 tup))
	     C.Param.([|int32 et|])
	     Map.empty

    let inclusion_succs, inclusion_succs_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.fold Q.inclusion_type_succs
	     C.Tuple.(fun tup -> Map.add (int32 0 tup) (mult 1 tup, mult 2 tup))
	     C.Param.([|int32 et|])
	     Map.empty

    let attribution, attribution_cache =
      memo_2lwt @@ fun (et, et') ->
      with_db @@ fun (module C) ->
      let aux tup akm =
	lwt ak = Attribute_key.of_id (C.Tuple.int32 0 tup) in
	let mult = Multiplicity.of_int (C.Tuple.int 1 tup) in
	Lwt.return (Attribute_key.Map.add ak mult akm) in
      C.fold_s Q.attribution_type aux C.Param.([|int32 et; int32 et'|])
	       Attribute_key.Map.empty

(*
    let attribution_preds, attribution_preds_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      let aux tup etm =
	let et = C.Tuple.int32 0 tup in
	lwt ak = Attribute_key.of_id (C.Tuple.int32 1 tup) in
	let mult = Multiplicity.of_int (C.Tuple.int 2 tup) in
	let akm = try Map.find et etm
		  with Not_found -> Attribute_key.Map.empty in
	Lwt.return (Map.add et (Attribute_key.Map.add ak mult akm) etm) in
      C.fold_s Q.attribution_type_preds aux C.Param.([|int32 et|]) Map.empty

    let attribution_succs, attribution_succs_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      let aux tup etm =
	let et = C.Tuple.int32 0 tup in
	lwt ak = Attribute_key.of_id (C.Tuple.int32 1 tup) in
	let mult = Multiplicity.of_int (C.Tuple.int 2 tup) in
	let akm = try Map.find et etm
		  with Not_found -> Attribute_key.Map.empty in
	Lwt.return (Map.add et (Attribute_key.Map.add ak mult akm) etm) in
      C.fold_s Q.attribution_type_succs aux C.Param.([|int32 et|]) Map.empty
*)

    let display_name ~langs ?pl = name (* FIXME *)
  end

  module Entity = struct
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    let compare = Int32.compare

    let of_id e = Lwt.return e
    let id e = e

    let type_, type_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      C.find Q.entity_type C.Tuple.(int32 0) C.Param.([|int32 e|])

    let viewer, viewer_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      C.find Q.entity_viewer C.Tuple.(int32 0) C.Param.([|int32 e|])

    let admin, admin_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      C.find Q.entity_admin C.Tuple.(int32 0) C.Param.([|int32 e|])

    let minimums, minimums_cache = memo_0lwt @@ fun () ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.minimums add [||] Set.empty

    let maximums, maximums_cache = memo_0lwt @@ fun () ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.maximums add [||] Set.empty

    let preds, preds_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.entity_preds add C.Param.([|int32 e|]) Set.empty

    let succs, succs_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.entity_succs add C.Param.([|int32 e|]) Set.empty

    let create ~viewer ~admin entity_type =
      with_db @@ fun (module C) ->
	C.find Q.create_entity C.Tuple.(int32 0)
	       C.Param.([|int32 entity_type; int32 viewer; int32 admin|])

    let precedes subentity superentity =
      if subentity = superentity then Lwt.return_true else
      let k = subentity, superentity in
      try Lwt.return (Prime_cache.find inclusion_cache k)
      with Not_found ->
	lwt c = with_db @@ fun (module C) ->
	  C.find_opt Q.select_precedes (fun _ -> ())
		     C.Param.([|int32 subentity; int32 superentity|]) >|=
	  function None -> false | Some () -> true in
	Prime_cache.replace inclusion_cache preceq_grade k c;
	Lwt.return c

    let constrain' subentity superentity (module C : CONNECTION) =
      (* FIXME: Invalidate cache entries. *)
      C.exec Q.insert_inclusion
	C.Param.([|int32 subentity; int32 superentity|])

    let unconstrain' subentity superentity (module C : CONNECTION) =
      (* FIXME: Invalidate cache entries. *)
      C.exec Q.delete_inclusion
	C.Param.([|int32 subentity; int32 superentity|])

    let constrain subentity superentity =
      lwt is_sub = precedes subentity superentity in
      if is_sub then Lwt.return_unit else
      lwt is_super = precedes superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      (* TODO: Update entity_rank. *)
      with_db (constrain' subentity superentity)
      (* TODO: Update is_subsumed. *)
      (* FIXME: Clear or update pred and succ caches. *)
      (* FIXME: Clear or update inclusion cache. *)

    let unconstrain subentity superentity =
      (* TODO: Update is_subsumed. *)
      with_db (unconstrain' subentity superentity)
      (* TODO: Update entity_rank. *)
      (* FIXME: Clear or update pred and succ caches. *)
      (* FIXME: Clear or update inclusion cache. *)

    type ptuple =
      Ptuple : (module Caqti_sigs.TUPLE with type t = 't) * 't -> ptuple

    let fetch_attribute (type a) e e' (ak : a Attribute_key.t1) =
      let open Attribute_key in
      let aux cache q (detuple : _ -> a) : a list Lwt.t =
	try Lwt.return (Prime_cache.find cache (e, e', Ex ak))
	with Not_found ->
	  with_db @@ fun (module C : CONNECTION) ->
	  let p = C.Param.([|int32 e; int32 e'; int32 ak.ak_id|]) in
	  let push tup acc = detuple (Ptuple ((module C.Tuple), tup)) :: acc in
	  lwt r = C.fold q push p [] in
	  Prime_cache.replace cache attribution_grade (e, e', Ex ak) r;
	  Lwt.return r in
      match ak.ak_value_type with
      | Type.Bool ->
	aux bool_attribute_cache Q.select_integer_attribution
	    (fun (Ptuple ((module T), tup)) -> T.int 0 tup <> 0)
      | Type.Int ->
	aux int_attribute_cache Q.select_integer_attribution
	    (fun (Ptuple ((module T), tup)) -> T.int 0 tup)
      | Type.String ->
	aux string_attribute_cache Q.select_text_attribution
	    (fun (Ptuple ((module T), tup)) -> T.text 0 tup)
      | Type.Twine -> assert false (* FIXME *)

    let store_attribute e e' ak av = assert false (* FIXME *)

    let display_name ~langs e =
      (* FIXME *)
      Lwt.return @@ Printf.sprintf "Entity # %ld" e
  end

end : S)
