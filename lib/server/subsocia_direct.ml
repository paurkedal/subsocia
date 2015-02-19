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
let attribute_type_grade = fetch_grade
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

  let attribute_type_by_id =
    q "SELECT attribute_name, value_type FROM @attribute_type \
       WHERE attribute_type_id = ?"
  let attribute_type_by_name =
    q "SELECT attribute_type_id, value_type FROM @attribute_type \
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
    q "SELECT attribute_type_id, attribute_multiplicity \
       FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"
  let attribution_mult =
    q "SELECT attribute_multiplicity FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ? \
	 AND attribute_type_id = ?"
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
       WHERE subentity_id = ? AND superentity_id = ? AND attribute_type_id = ?"
  let select_integer_attribution =
    q "SELECT value FROM @integer_attribution \
       WHERE subentity_id = ? AND superentity_id = ? AND attribute_type_id = ?"

  let insert_text_attribution =
    q "INSERT INTO @text_attribution \
	(subentity_id, superentity_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"
  let insert_integer_attribution =
    q "INSERT INTO @integer_attribution \
	(subentity_id, superentity_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"

  let delete_text_attribution =
    q "DELETE FROM @text_attribution \
       WHERE subentity_id = ? AND superentity_id = ? \
	 AND attribute_type_id = ? AND value = ?"
  let delete_integer_attribution =
    q "DELETE FROM @text_attribution \
       WHERE subentity_id = ? AND superentity_id = ? \
	 AND attribute_type_id = ? AND value = ?"

  let apreds_text =
    q "SELECT subentity_id FROM @text_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ? AND value = ?"
  let apreds_integer =
    q "SELECT subentity_id FROM @integer_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ? AND value = ?"

  let asuccs_text =
    q "SELECT superentity_id FROM @text_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ? AND value = ?"
  let asuccs_integer =
    q "SELECT superentity_id FROM @integer_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ? AND value = ?"
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
  type attribute_type_id = int32
  type entity_type_id = int32
  type entity_id = int32

  module Id_set = Prime_enumset.Make (Int32)
  module Id_map = Prime_enummap.Make (Int32)

  module Attribute_type_base = struct
    type 'a t1 = {
      ak_id : attribute_type_id;
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

  let inclusion_cache = Prime_cache.create ~cache_metric 61

  let bool_attribute_cache :
    (entity_id * entity_id * attribute_type_id, bool Values.t) Prime_cache.t =
    Prime_cache.create ~cache_metric 61
  let int_attribute_cache :
    (entity_id * entity_id * attribute_type_id, int Values.t) Prime_cache.t =
    Prime_cache.create ~cache_metric 61
  let string_attribute_cache :
    (entity_id * entity_id * attribute_type_id, string Values.t) Prime_cache.t =
    Prime_cache.create ~cache_metric 61

  let pool =
    let connect () = Caqti_lwt.connect uri in
    let disconnect (module C : CONNECTION) = C.disconnect () in
    let validate (module C : CONNECTION) = C.validate () in
    let check (module C : CONNECTION) = C.check in
    Caqti_lwt.Pool.create ~validate ~check connect disconnect

  let with_db f = Caqti_lwt.Pool.use f pool

  module Attribute_type = struct
    include Attribute_type_base

    module Comparable = struct
      type t = t0
      let compare (Ex x) (Ex y) = compare x.ak_id y.ak_id
    end
    module Map = Prime_enummap.Make (Comparable)
    module Set = Prime_enumset.Make (Comparable)

    let id (Ex ak) = ak.ak_id
    let name (Ex ak) = Lwt.return ak.ak_name

    let of_id, of_id_cache = memo_1lwt @@ fun ak_id ->
      with_db @@ fun (module C : CONNECTION) ->
      C.find Q.attribute_type_by_id
	     C.Tuple.(fun tup -> text 0 tup, text 1 tup)
	     C.Param.([|int32 ak_id|]) >|= fun (ak_name, value_type) ->
      let Type.Ex ak_value_type = Type.of_string value_type in
      Beacon.embed attribute_type_grade @@ fun ak_beacon ->
      Ex {ak_id; ak_name; ak_value_type; ak_beacon}

    let of_name, of_name_cache = memo_1lwt @@ fun ak_name ->
      with_db @@ fun (module C : CONNECTION) ->
      C.find_opt Q.attribute_type_by_name
		 C.Tuple.(fun tup -> int32 0 tup, text 1 tup)
		 C.Param.([|text ak_name|]) >|=
      Option.map begin fun (ak_id, value_type) ->
	let Type.Ex ak_value_type = Type.of_string value_type in
	Beacon.embed attribute_type_grade @@ fun ak_beacon ->
	  (Ex {ak_id; ak_name; ak_value_type; ak_beacon})
      end
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
	lwt ak = Attribute_type.of_id (C.Tuple.int32 0 tup) in
	let mult = Multiplicity.of_int (C.Tuple.int 1 tup) in
	Lwt.return (Attribute_type.Map.add ak mult akm) in
      C.fold_s Q.attribution_type aux C.Param.([|int32 et; int32 et'|])
	       Attribute_type.Map.empty

    let attribution_mult', attribution_mult_cache =
      memo_3lwt @@ fun (et, et', ak) ->
      with_db @@ fun (module C) ->
      let aux tup = Multiplicity.of_int (C.Tuple.int 0 tup) in
      C.find_opt Q.attribution_mult aux
		 C.Param.([|int32 et; int32 et'; int32 ak|])

    let attribution_mult0 et et' (Attribute_type.Ex ak) =
      attribution_mult' et et' ak.Attribute_type.ak_id

    let attribution_mult1 et et' ak =
      attribution_mult' et et' ak.Attribute_type.ak_id

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

    let getattr (type a) e e' (ak : a Attribute_type.t1) =
      let open Attribute_type in
      let aux cache q (detuple : _ -> a) : a Values.t Lwt.t =
	try Lwt.return (Prime_cache.find cache (e, e', ak.ak_id))
	with Not_found ->
	  with_db @@ fun (module C : CONNECTION) ->
	  let p = C.Param.([|int32 e; int32 e'; int32 ak.ak_id|]) in
	  let push tup acc =
	    Values.add (detuple (Ptuple ((module C.Tuple), tup))) acc in
	  lwt r = C.fold q push p (Values.empty ak.ak_value_type) in
	  Prime_cache.replace cache attribution_grade (e, e', ak.ak_id) r;
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

    let addattr' (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      with_db @@ fun (module C : CONNECTION) ->
      let aux q conv =
	Lwt_list.iter_s
	  (fun x ->
	    let p = C.Param.([|int32 e; int32 e';
			       int32 ak.Attribute_type.ak_id; conv x|]) in
	    C.exec q p)
	  xs in
      match ak.Attribute_type.ak_value_type with
      | Type.Bool -> aux Q.insert_integer_attribution
		     (fun x -> C.Param.int (if x then 1 else 0))
      | Type.Int -> aux Q.insert_integer_attribution C.Param.int
      | Type.String -> aux Q.insert_text_attribution C.Param.text

    let addattr (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      lwt mu = Entity_type.attribution_mult1 e e' ak in
      lwt xs_pres = getattr e e' ak in
      let xs =
	match mu with
	| None -> failwith "addattr: Not allowed between these elements."
	| Some Multiplicity.May1 | Some Multiplicity.Must1 ->
	  if not (Values.is_empty xs_pres) then
	    invalid_arg "addattr: Attribute already set.";
	  xs
	| Some Multiplicity.May | Some Multiplicity.Must ->
	  let ht = Hashtbl.create 7 in
	  Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
	  List.filter
	    (fun x -> if Hashtbl.mem ht x then false else
		      (Hashtbl.add ht x (); true)) xs in
      if xs = [] then Lwt.return_unit else
      addattr' e e' ak xs

    let delattr' (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      with_db @@ fun (module C : CONNECTION) ->
      let aux q conv =
	Lwt_list.iter_s
	  (fun x ->
	    let p = C.Param.([|int32 e; int32 e';
			       int32 ak.Attribute_type.ak_id; conv x|]) in
	    C.exec q p)
	  xs in
      match ak.Attribute_type.ak_value_type with
      | Type.Bool -> aux Q.delete_integer_attribution
		     (fun x -> C.Param.int (if x then 1 else 0))
      | Type.Int -> aux Q.delete_integer_attribution C.Param.int
      | Type.String -> aux Q.delete_text_attribution C.Param.text

    let delattr (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      lwt xs_pres = getattr e e' ak in
      let xs =
	let ht = Hashtbl.create 7 in
	Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
	List.filter
	  (fun x -> if not (Hashtbl.mem ht x) then false else
		    (Hashtbl.remove ht x; true)) xs in
      if xs = [] then Lwt.return_unit else
      delattr' e e' ak xs

    let setattr (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      lwt xs_pres = getattr e e' ak in
      let ht = Hashtbl.create 7 in
      Values.iter (fun x -> Hashtbl.add ht x false) xs_pres;
      let xs_ins =
	List.filter
	  (fun x ->
	    try
	      if Hashtbl.find ht x then false else
	      (Hashtbl.replace ht x true; true)
	    with Not_found ->
	      (Hashtbl.add ht x true; true))
	  xs in
      let xs_del =
	Hashtbl.fold (fun x ins acc -> if ins then acc else x :: acc) ht [] in
      (if xs_del = [] then Lwt.return_unit else delattr' e e' ak xs_del) >>
      (if xs_ins = [] then Lwt.return_unit else addattr' e e' ak xs_ins)

    let apreds_integer, apreds_integer_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.apreds_integer f C.Param.([|int32 e; int32 ak_id; int x|])
	     Set.empty

    let apreds_text, apreds_text_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.apreds_text f C.Param.([|int32 e; int32 ak_id; text x|])
	     Set.empty

    let apreds (type a) e (ak : a Attribute_type.t1) : a -> Set.t Lwt.t =
      let open Attribute_type in
      match type1 ak with
      | Type.Bool -> fun x -> apreds_integer e ak.ak_id (if x then 1 else 0)
      | Type.Int -> apreds_integer e ak.ak_id
      | Type.String -> apreds_text e ak.ak_id

    let asuccs_integer, asuccs_integer_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.asuccs_integer f C.Param.([|int32 e; int32 ak_id; int x|])
	     Set.empty

    let asuccs_text, asuccs_text_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.asuccs_text f C.Param.([|int32 e; int32 ak_id; text x|])
	     Set.empty

    let asuccs (type a) e (ak : a Attribute_type.t1) : a -> Set.t Lwt.t =
      let open Attribute_type in
      match type1 ak with
      | Type.Bool -> fun x -> asuccs_integer e ak.ak_id (if x then 1 else 0)
      | Type.Int -> asuccs_integer e ak.ak_id
      | Type.String -> asuccs_text e ak.ak_id

  end

end : S)
