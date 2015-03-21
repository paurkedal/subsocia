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

module type S = Subsocia_direct_intf.S

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
  let attribute_type_create =
    q "INSERT INTO @attribute_type (attribute_name, value_type) \
       VALUES (?, ?) RETURNING attribute_type_id"
  let attribute_type_delete =
    q "DELETE FROM @attribute_type WHERE attribute_type_id = ?"

  (* Entity types *)

  let entity_type_id_of_name =
    q "SELECT entity_type_id FROM @entity_type WHERE entity_type_name = ?"
  let entity_type_name_of_id =
    q "SELECT entity_type_name FROM @entity_type WHERE entity_type_id = ?"
  let entity_type_create =
    q "INSERT INTO @entity_type (entity_type_name) VALUES (?) \
       RETURNING entity_type_id"
  let entity_type_delete =
    q "DELETE FROM @entity_type WHERE entity_type_id = ?"
  let entity_type_all =
    q "SELECT entity_type_id FROM @entity_type"
  let entity_name_tmpl =
    q "SELECT entity_name_tmpl FROM @entity_type WHERE entity_type_id = ?"
  let set_entity_name_tmpl =
    q "UPDATE @entity_type SET entity_name_tmpl = ? WHERE entity_type_id = ?"

  let inclusion_type =
    q "SELECT subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"
  let inclusion_type_preds =
    q "SELECT subentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type WHERE superentity_type_id = ?"
  let inclusion_type_succs =
    q "SELECT superentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type WHERE subentity_type_id = ?"
  let inclusion_type_dump =
    q "SELECT subentity_type_id, superentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type"
  let inclusion_type_allow =
    q "INSERT INTO @inclusion_type \
	(subentity_multiplicity, superentity_multiplicity, \
	 subentity_type_id, superentity_type_id) \
       VALUES (?, ?, ?, ?)"
  let inclusion_type_disallow =
    q "DELETE FROM @inclusion_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"

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
  let attribution_type_dump =
    q "SELECT subentity_type_id, superentity_type_id, \
	      attribute_type_id, attribute_multiplicity \
       FROM @attribution_type"
  let attribution_allow =
    q "INSERT INTO @attribution_type \
	(subentity_type_id, superentity_type_id, \
	 attribute_type_id, attribute_multiplicity) \
       VALUES (?, ?, ?, ?)"
  let attribution_disallow =
    q "DELETE FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ? \
	 AND attribute_type_id = ?"

  (* Entites *)

  let inclusion_succs =
    q "SELECT superentity_id FROM @inclusion WHERE subentity_id = ?"
  let inclusion_preds =
    q "SELECT subentity_id FROM @inclusion WHERE superentity_id = ?"

  let entity_type = q "SELECT entity_type_id FROM @entity WHERE entity_id = ?"
  let entity_viewer = q "SELECT viewer_id FROM @entity WHERE entity_id = ?"
  let entity_admin = q "SELECT admin_id FROM @entity WHERE entity_id = ?"

  let type_members =
    q "SELECT entity_id FROM @entity WHERE entity_type_id = ?"

  let minimums =
    q "SELECT entity_id FROM @entity \
       WHERE NOT EXISTS \
	(SELECT 0 FROM @inclusion WHERE superentity_id = entity_id)"

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

  let set_entity_admin =
    q "UPDATE @entity SET admin_id = ? WHERE entity_id = ?"
  let set_entity_viewer =
    q "UPDATE @entity SET viewer_id = ? WHERE entity_id = ?"

  let delete_entity =
    q "DELETE FROM @entity WHERE entity_id = ?"

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

  let getattrpreds_text =
    q "SELECT subentity_id, value FROM @text_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ?"
  let getattrpreds_integer =
    q "SELECT subentity_id, value FROM @integer_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ?"

  let getattrsuccs_text =
    q "SELECT superentity_id, value FROM @text_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ?"
  let getattrsuccs_integer =
    q "SELECT superentity_id, value FROM @integer_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ?"
end

module type CACHE = sig
  type ('a, 'b) t
  val create :  cache_metric: Prime_cache_metric.t -> int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val find : ('a, 'b) t -> 'a -> 'b
  val find_o : ('a, 'b) t -> 'a -> 'b option
  val replace : ('a, 'b) t -> float -> 'a -> 'b -> unit
  val remove : ('a, 'b) t -> 'a -> unit
  val memo_lwt : ('a -> 'b Lwt.t) -> ('a -> 'b Lwt.t) * ('a, 'b) t
end

module Enabled_cache = struct
  include Prime_cache

  let memo_lwt f =
    let cache = Prime_cache.create ~cache_metric 23 in
    let g x =
      try Lwt.return (Prime_cache.find cache x)
      with Not_found ->
	lwt y = f x in
	Prime_cache.replace cache fetch_grade x y;
	Lwt.return y in
    g, cache
end

module Disabled_cache = struct
  type ('a, 'b) t = unit
  let create ~cache_metric n = ()
  let clear _ = ()
  let find _ _ = raise Not_found
  let find_o _ _ = None
  let replace _ _ _ _ = ()
  let remove _ _ = ()
  let memo_lwt f = f, ()
end

module Cache =
  (val if Subsocia_config.enable_caching#get
       then (module Enabled_cache)
       else (module Disabled_cache) : CACHE)

let memo_1lwt = Cache.memo_lwt
let memo_0lwt = memo_1lwt
let memo_2lwt f = let g, c = memo_1lwt f in (fun x0 x1 -> g (x0, x1)), c
let memo_3lwt f = let g, c = memo_1lwt f in (fun x0 x1 x2 -> g (x0, x1, x2)), c

module Base = struct
  type attribute_type_id = int32
  type entity_type_id = int32
  type entity_id = int32

  module Id_set = Int32_set
  module Id_map = Int32_map

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

  let inclusion_cache = Cache.create ~cache_metric 61

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
    module Map = Prime_enummap.Make_monadic (Comparable) (Lwt)
    module Set = Prime_enumset.Make_monadic (Comparable) (Lwt)

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

    let create vt ak_name =
      with_db @@ fun (module C : CONNECTION) ->
      C.find Q.attribute_type_create C.Tuple.(int32 0)
	     C.Param.([|text ak_name; text (Type.string_of_t0 vt)|]) >>= of_id

    let delete (Ex ak) =
      with_db @@ fun (module C : CONNECTION) ->
      C.exec Q.attribute_type_delete C.Param.([|int32 ak.ak_id|])
  end

  module Attribute = struct
    type t0 = Ex : 'a Attribute_type.t1 * 'a -> t0
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

    let create etn =
      with_db @@ fun (module C) ->
      C.find Q.entity_type_create C.Tuple.(int32 0) C.Param.([|text etn|])

    let delete et =
      with_db @@ fun (module C) ->
      C.exec Q.entity_type_delete C.Param.([|int32 et|])

    let all () =
      with_db @@ fun (module C) ->
      C.fold Q.entity_type_all (fun tup -> Set.add (C.Tuple.int32 0 tup)) [||]
	     Set.empty

    let entity_name_tmpl, entity_name_tmpl_cache = memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      C.find Q.entity_name_tmpl C.Tuple.(text 0) C.Param.([|int32 et|])

    let set_entity_name_tmpl et name =
      with_db @@ fun (module C) ->
      C.exec Q.set_entity_name_tmpl C.Param.([|text name; int32 et|])

    let inclusion, inclusion_cache =
      memo_2lwt @@ fun (et0, et1) ->
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.find_opt Q.inclusion_type C.Tuple.(fun tup -> mult 0 tup, mult 1 tup)
		 C.Param.([|int32 et0; int32 et1|])

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

    let inclusion_dump () =
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.fold Q.inclusion_type_dump
	     C.Tuple.(fun tup acc -> (int32 0 tup, int32 1 tup,
				      mult 2 tup, mult 3 tup) :: acc)
	     [||] []

    let inclusion_allow mu0 mu1 et0 et1 =
      with_db @@ fun (module C) ->
      let mu0, mu1 = Multiplicity.(to_int mu0, to_int mu1) in
      C.exec Q.inclusion_type_allow
	     C.Param.([|int mu0; int mu1; int32 et0; int32 et1|])

    let inclusion_disallow et0 et1 =
      with_db @@ fun (module C) ->
      C.exec Q.inclusion_type_disallow C.Param.([|int32 et0; int32 et1|])

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

    let attribution_dump () =
      with_db @@ fun (module C) ->
      let aux tup acc =
	let et0, et1 = C.Tuple.(int32 0 tup, int32 1 tup) in
	let mu = Multiplicity.of_int C.Tuple.(int 3 tup) in
	Attribute_type.of_id C.Tuple.(int32 2 tup) >|= fun ak ->
	(et0, et1, ak, mu) :: acc in
      C.fold_s Q.attribution_type_dump aux [||] []

    let attribution_allow et et' (Attribute_type.Ex ak) mu =
      with_db @@ fun (module C) ->
      let mu = Multiplicity.to_int mu in
      C.exec Q.attribution_allow
	     C.Param.([|int32 et; int32 et'; int32 ak.Attribute_type.ak_id;
			int mu|])

    let attribution_disallow et et' (Attribute_type.Ex ak) =
      with_db @@ fun (module C) ->
      C.exec Q.attribution_disallow
	     C.Param.([|int32 et; int32 et'; int32 ak.Attribute_type.ak_id|])

    let display_name ~langs ?pl = name (* FIXME *)
  end

  module Entity = struct
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    let changed_event_table = Int32_event_table.create 97
    let emit_changed = Int32_event_table.emit changed_event_table

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

    let type_members, type_members_cache = memo_1lwt @@ fun entity_type_id ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.type_members add C.Param.([|int32 entity_type_id|]) Set.empty

    let top = of_id 1l

    let minimums, minimums_cache = memo_0lwt @@ fun () ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.minimums add [||] Set.empty

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

    let modify ?viewer ?admin e =
      with_db @@ fun (module C) ->
      Pwt_option.iter_s
	(fun viewer ->
	  C.exec Q.set_entity_viewer C.Param.([|int32 viewer; int32 e|]) >>
	  Lwt.return (Cache.remove viewer_cache e))
	viewer >>
      Pwt_option.iter_s
	(fun admin ->
	  C.exec Q.set_entity_admin C.Param.([|int32 admin; int32 e|]) >>
	  Lwt.return (Cache.remove admin_cache e))
	admin

    let delete e =
      with_db @@ fun (module C) ->
      C.exec Q.delete_entity C.Param.([|int32 e|]) >|= fun () ->
      Cache.clear minimums_cache;
      Cache.clear preds_cache

    let precedes subentity superentity =
      if subentity = superentity then Lwt.return_true else
      let k = subentity, superentity in
      try Lwt.return (Cache.find inclusion_cache k)
      with Not_found ->
	lwt c = with_db @@ fun (module C) ->
	  C.find_opt Q.select_precedes (fun _ -> ())
		     C.Param.([|int32 subentity; int32 superentity|]) >|=
	  function None -> false | Some () -> true in
	Cache.replace inclusion_cache preceq_grade k c;
	Lwt.return c

    let clear_inclusion_caches () =
      Cache.clear minimums_cache;
      Cache.clear preds_cache;
      Cache.clear succs_cache;
      Cache.clear inclusion_cache

    type ptuple =
      Ptuple : (module Caqti_sigs.TUPLE with type t = 't) * 't -> ptuple

    let getattr_integer, getattr_integer_cache =
      memo_3lwt @@ fun (e, e', ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.select_integer_attribution
	     C.Tuple.(fun tup -> Values.add (int 0 tup))
	     C.Param.([|int32 e; int32 e'; int32 ak_id|])
	     (Values.empty Type.Int)

    let getattr_text, getattr_text_cache =
      memo_3lwt @@ fun (e, e', ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.select_text_attribution
	     C.Tuple.(fun tup -> Values.add (text 0 tup))
	     C.Param.([|int32 e; int32 e'; int32 ak_id|])
	     (Values.empty Type.String)

    let getattr (type a) e e' (ak : a Attribute_type.t1) : a Values.t Lwt.t =
      match Attribute_type.type1 ak with
      | Type.Bool ->
	getattr_integer e e' ak.Attribute_type.ak_id >|= fun s ->
	Values.fold (Values.add *< (<>) 0) s (Values.empty Type.Bool)
      | Type.Int -> getattr_integer e e' ak.Attribute_type.ak_id
      | Type.String -> getattr_text e e' ak.Attribute_type.ak_id

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

    let getattrpreds_integer, getattrpreds_integer_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, int 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
	Map.add e' (Values.add v vs) m in
      C.fold Q.getattrpreds_integer f C.Param.([|int32 e; int32 ak_id|])
	     Map.empty

    let getattrpreds_text, getattrpreds_text_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, text 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
	Map.add e' (Values.add v vs) m in
      C.fold Q.getattrpreds_text f C.Param.([|int32 e; int32 ak_id|]) Map.empty

    let getattrpreds (type a) e (ak : a Attribute_type.t1)
	  : a Values.t Map.t Lwt.t =
      match Attribute_type.type1 ak with
      | Type.Bool ->
	lwt m = getattrpreds_integer e ak.Attribute_type.ak_id in
	let t = Attribute_type.type1 ak in
	let aux vs = Values.fold (Values.add *< ((<>) 0)) vs (Values.empty t) in
	Lwt.return (Map.map aux m)
      | Type.Int -> getattrpreds_integer e ak.Attribute_type.ak_id
      | Type.String -> getattrpreds_text e ak.Attribute_type.ak_id

    let getattrsuccs_integer, getattrsuccs_integer_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, int 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
	Map.add e' (Values.add v vs) m in
      C.fold Q.getattrsuccs_integer f C.Param.([|int32 e; int32 ak_id|])
	     Map.empty

    let getattrsuccs_text, getattrsuccs_text_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, text 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
	Map.add e' (Values.add v vs) m in
      C.fold Q.getattrsuccs_text f C.Param.([|int32 e; int32 ak_id|]) Map.empty

    let getattrsuccs (type a) e (ak : a Attribute_type.t1)
	  : a Values.t Map.t Lwt.t =
      match Attribute_type.type1 ak with
      | Type.Bool ->
	lwt m = getattrsuccs_integer e ak.Attribute_type.ak_id in
	let t = Attribute_type.type1 ak in
	let aux vs = Values.fold (Values.add *< ((<>) 0)) vs (Values.empty t) in
	Lwt.return (Map.map aux m)
      | Type.Int -> getattrsuccs_integer e ak.Attribute_type.ak_id
      | Type.String -> getattrsuccs_text e ak.Attribute_type.ak_id

    let clear_integer_caches () =
      Cache.clear getattr_integer_cache;
      Cache.clear apreds_integer_cache;
      Cache.clear asuccs_integer_cache;
      Cache.clear getattrpreds_integer_cache;
      Cache.clear getattrsuccs_integer_cache

    let clear_text_caches () =
      Cache.clear getattr_text_cache;
      Cache.clear apreds_text_cache;
      Cache.clear asuccs_text_cache;
      Cache.clear getattrpreds_text_cache;
      Cache.clear getattrsuccs_text_cache

    let clear_attr_caches (type a) (ak : a Attribute_type.t1) : unit =
      match Attribute_type.type1 ak with
      | Type.Bool -> clear_integer_caches ()
      | Type.Int -> clear_integer_caches ()
      | Type.String -> clear_text_caches ()

    (* Modifying Functions *)

    let constrain' subentity superentity (module C : CONNECTION) =
      C.exec Q.insert_inclusion
	C.Param.([|int32 subentity; int32 superentity|]) >|=
      fun () ->
	clear_inclusion_caches ();
	emit_changed subentity `Succ;
	emit_changed superentity `Pred

    let unconstrain' subentity superentity (module C : CONNECTION) =
      C.exec Q.delete_inclusion
	C.Param.([|int32 subentity; int32 superentity|]) >|=
      fun () ->
	clear_inclusion_caches ();
	emit_changed subentity `Succ;
	emit_changed superentity `Pred

    let constrain subentity superentity =
      lwt is_sub = precedes subentity superentity in
      if is_sub then Lwt.return_unit else
      lwt is_super = precedes superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      (* TODO: Update entity_rank. *)
      with_db (constrain' subentity superentity)
      (* TODO: Update is_subsumed. *)

    let unconstrain subentity superentity =
      (* TODO: Update is_subsumed. *)
      with_db (unconstrain' subentity superentity)
      (* TODO: Update entity_rank. *)

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
      addattr' e e' ak xs >|=
      fun () ->
	clear_attr_caches ak;
	emit_changed e `Asucc;
	emit_changed e' `Apred

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
      delattr' e e' ak xs >|=
      fun () ->
	clear_attr_caches ak;
	emit_changed e `Asucc;
	emit_changed e' `Apred

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
      (if xs_ins = [] then Lwt.return_unit else addattr' e e' ak xs_ins) >|=
      fun () ->
	clear_attr_caches ak;
	emit_changed e `Asucc;
	emit_changed e' `Apred

  end

  let entity_changed = Int32_event_table.event Entity.changed_event_table

end : S)
