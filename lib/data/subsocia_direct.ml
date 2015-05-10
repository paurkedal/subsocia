(* Copyright (C) 2014--2015  Petter A. Urkedal <paurkedal@gmail.com>
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
open Pwt_infix
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

  let begin_   = prepare_sql "BEGIN"
  let commit   = prepare_sql "COMMIT"
  let rollback = prepare_sql "ROLLBACK"

  (* Attribute key *)

  let at_by_id =
    q "SELECT attribute_name, value_type FROM @attribute_type \
       WHERE attribute_type_id = ?"
  let at_by_name =
    q "SELECT attribute_type_id, value_type FROM @attribute_type \
       WHERE attribute_name = ?"
  let at_create =
    q "INSERT INTO @attribute_type (attribute_name, value_type) \
       VALUES (?, ?) RETURNING attribute_type_id"
  let at_delete =
    q "DELETE FROM @attribute_type WHERE attribute_type_id = ?"

  (* Entity types *)

  let et_id_of_name =
    q "SELECT entity_type_id FROM @entity_type WHERE entity_type_name = ?"
  let et_name_of_id =
    q "SELECT entity_type_name FROM @entity_type WHERE entity_type_id = ?"
  let et_create =
    q "INSERT INTO @entity_type (entity_type_name) VALUES (?) \
       RETURNING entity_type_id"
  let et_delete =
    q "DELETE FROM @entity_type WHERE entity_type_id = ?"
  let et_all =
    q "SELECT entity_type_id FROM @entity_type"
  let et_entity_name_tmpl =
    q "SELECT entity_name_tmpl FROM @entity_type WHERE entity_type_id = ?"
  let et_set_entity_name_tmpl =
    q "UPDATE @entity_type SET entity_name_tmpl = ? WHERE entity_type_id = ?"

  let et_can_dsub =
    q "SELECT subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"
  let et_dsub =
    q "SELECT subentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type WHERE superentity_type_id = ?"
  let et_dsuper =
    q "SELECT superentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type WHERE subentity_type_id = ?"
  let et_dsub_elements =
    q "SELECT subentity_type_id, superentity_type_id, \
	      subentity_multiplicity, superentity_multiplicity \
       FROM @inclusion_type"
  let et_allow_dsub =
    q "INSERT INTO @inclusion_type \
	(subentity_multiplicity, superentity_multiplicity, \
	 subentity_type_id, superentity_type_id) \
       VALUES (?, ?, ?, ?)"
  let et_disallow_dsub =
    q "DELETE FROM @inclusion_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"

  let et_can_asub_byattr =
    q "SELECT attribute_type_id, attribute_multiplicity \
       FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ?"
  let et_can_asub =
    q "SELECT attribute_multiplicity FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ? \
	 AND attribute_type_id = ?"
  let et_asub_elements =
    q "SELECT subentity_type_id, superentity_type_id, \
	      attribute_type_id, attribute_multiplicity \
       FROM @attribution_type"
  let et_allow_asub =
    q "INSERT INTO @attribution_type \
	(subentity_type_id, superentity_type_id, \
	 attribute_type_id, attribute_multiplicity) \
       VALUES (?, ?, ?, ?)"
  let et_disallow_asub =
    q "DELETE FROM @attribution_type \
       WHERE subentity_type_id = ? AND superentity_type_id = ? \
	 AND attribute_type_id = ?"

  (* Entites *)

  let e_dsuper =
    q "SELECT superentity_id FROM @inclusion WHERE subentity_id = ?"
  let e_dsub =
    q "SELECT subentity_id FROM @inclusion WHERE superentity_id = ?"

  let e_type = q "SELECT entity_type_id FROM @entity WHERE entity_id = ?"
  let e_rank = q "SELECT entity_rank FROM @entity WHERE entity_id = ?"
  let e_access = q "SELECT access_id FROM @entity WHERE entity_id = ?"

  let e_set_entity_rank =
    q "UPDATE @entity SET entity_rank = ? WHERE entity_id = ?"

  let e_type_members =
    q "SELECT entity_id FROM @entity WHERE entity_type_id = ?"

  let e_minimums =
    q "SELECT entity_id FROM @entity \
       WHERE NOT EXISTS \
	(SELECT 0 FROM @inclusion WHERE superentity_id = entity_id)"

  let e_dsub =
    q "SELECT entity_id FROM @entity JOIN @inclusion \
			  ON entity_id = subentity_id \
       WHERE superentity_id = ?"

  let e_dsuper =
    q "SELECT entity_id FROM @entity JOIN @inclusion \
			  ON entity_id = superentity_id \
       WHERE subentity_id = ?"

  let e_select_precedes =
    q "WITH RECURSIVE successors(entity_id) AS ( \
	  SELECT i.superentity_id AS entity_id \
	  FROM @inclusion i \
	  JOIN @entity e ON e.entity_id = i.superentity_id \
	  WHERE i.is_subsumed = false \
	    AND e.entity_rank >= ? \
	    AND i.subentity_id = ? \
	UNION \
	  SELECT i.superentity_id \
	  FROM @inclusion i \
	  JOIN @entity e ON e.entity_id = i.superentity_id \
	  JOIN successors c ON i.subentity_id = c.entity_id \
	  WHERE i.is_subsumed = false \
	    AND e.entity_rank >= ? \
       ) \
       SELECT 0 FROM successors WHERE entity_id = ? LIMIT 1"

  let e_create_entity =
    q "INSERT INTO @entity (entity_type_id, access_id) \
       VALUES (?, ?) RETURNING entity_id"

  let e_set_entity_access =
    q "UPDATE @entity SET access_id = ? WHERE entity_id = ?"

  let e_delete_entity =
    q "DELETE FROM @entity WHERE entity_id = ?"

  let e_maybe_insert_inclusion =
    q "INSERT INTO @inclusion (subentity_id, superentity_id) SELECT ?, ? \
       WHERE NOT EXISTS (SELECT 0 FROM @inclusion \
			 WHERE subentity_id = ? AND superentity_id = ?)"

  let e_delete_inclusion =
    q "DELETE FROM @inclusion WHERE subentity_id = ? AND superentity_id = ?"

  let e_select_text_attribution =
    q "SELECT value FROM @text_attribution \
       WHERE subentity_id = ? AND superentity_id = ? AND attribute_type_id = ?"
  let e_select_integer_attribution =
    q "SELECT value FROM @integer_attribution \
       WHERE subentity_id = ? AND superentity_id = ? AND attribute_type_id = ?"

  let e_insert_text_attribution =
    q "INSERT INTO @text_attribution \
	(subentity_id, superentity_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"
  let e_insert_integer_attribution =
    q "INSERT INTO @integer_attribution \
	(subentity_id, superentity_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"

  let e_delete_text_attribution =
    q "DELETE FROM @text_attribution \
       WHERE subentity_id = ? AND superentity_id = ? \
	 AND attribute_type_id = ? AND value = ?"
  let e_delete_integer_attribution =
    q "DELETE FROM @text_attribution \
       WHERE subentity_id = ? AND superentity_id = ? \
	 AND attribute_type_id = ? AND value = ?"

  let e_asub_eq_text =
    q "SELECT subentity_id FROM @text_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ? AND value = ?"
  let e_asub_eq_integer =
    q "SELECT subentity_id FROM @integer_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ? AND value = ?"

  let e_asuper_eq_text =
    q "SELECT superentity_id FROM @text_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ? AND value = ?"
  let e_asuper_eq_integer =
    q "SELECT superentity_id FROM @integer_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ? AND value = ?"

  let e_asub_get_text =
    q "SELECT subentity_id, value FROM @text_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ?"
  let e_asub_get_integer =
    q "SELECT subentity_id, value FROM @integer_attribution \
       WHERE superentity_id = ? AND attribute_type_id = ?"

  let e_asuper_get_text =
    q "SELECT superentity_id, value FROM @text_attribution \
       WHERE subentity_id = ? AND attribute_type_id = ?"
  let e_asuper_get_integer =
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
  val memo_lwt_conn : (?conn: 'c -> 'a -> 'b Lwt.t) ->
		      (?conn: 'c -> 'a -> 'b Lwt.t) * ('a, 'b) t
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

  let memo_lwt_conn f =
    let cache = Prime_cache.create ~cache_metric 23 in
    let g ?conn x =
      try Lwt.return (Prime_cache.find cache x)
      with Not_found ->
	lwt y = f ?conn x in
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
  let memo_lwt_conn f = f, ()
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

let rec make uri_or_conn = (module struct

  include Base

  let inclusion_cache = Cache.create ~cache_metric 61

  type with_db = {
    with_db : 'a. ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> 'a Lwt.t;
  }
  let with_db =
    match uri_or_conn with
    | `Uri uri ->
      let pool =
	let connect () = Caqti_lwt.connect uri in
	let disconnect (module C : CONNECTION) = C.disconnect () in
	let validate (module C : CONNECTION) = C.validate () in
	let check (module C : CONNECTION) = C.check in
	Caqti_lwt.Pool.create ~validate ~check connect disconnect in
      {with_db = fun f -> Caqti_lwt.Pool.use f pool}
    | `Connection conn ->
      let lock = Lwt_mutex.create () in
      {with_db = fun f -> Lwt_mutex.with_lock lock (fun () -> f conn)}
  let with_db ?conn f =
    match conn with
    | None -> with_db.with_db f
    | Some conn -> f conn

  let transaction f =
    with_db @@ fun ((module C : CONNECTION) as conn) ->
    C.exec Q.begin_ [||] >>= fun () ->
    try_lwt
      let module C' = (val make (`Connection conn) : S) in
      lwt r = f (module C' : Subsocia_intf.S) in
      C.exec Q.commit [||] >>
      Lwt.return r
    with exc ->
      Lwt_log.debug_f "Raised in transaction: %s" (Printexc.to_string exc) >>
      C.exec Q.rollback [||] >> Lwt.fail exc

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

    let of_id', of_id_cache = Cache.memo_lwt_conn @@ fun ?conn ak_id ->
      with_db ?conn @@ fun (module C : CONNECTION) ->
      C.find Q.at_by_id
	     C.Tuple.(fun tup -> text 0 tup, text 1 tup)
	     C.Param.([|int32 ak_id|]) >|= fun (ak_name, value_type) ->
      let Type.Ex ak_value_type = Type.of_string value_type in
      Beacon.embed attribute_type_grade @@ fun ak_beacon ->
      Ex {ak_id; ak_name; ak_value_type; ak_beacon}

    let of_id id = of_id' id

    let of_name, of_name_cache = memo_1lwt @@ fun ak_name ->
      with_db @@ fun (module C : CONNECTION) ->
      C.find_opt Q.at_by_name
		 C.Tuple.(fun tup -> int32 0 tup, text 1 tup)
		 C.Param.([|text ak_name|]) >|=
      Option.map begin fun (ak_id, value_type) ->
	let Type.Ex ak_value_type = Type.of_string value_type in
	Beacon.embed attribute_type_grade @@ fun ak_beacon ->
	  (Ex {ak_id; ak_name; ak_value_type; ak_beacon})
      end

    let create vt ak_name =
      with_db @@ fun ((module C : CONNECTION) as conn) ->
      C.find Q.at_create C.Tuple.(int32 0)
	     C.Param.([|text ak_name; text (Type.string_of_t0 vt)|])
	>>= of_id' ~conn

    let delete (Ex ak) =
      with_db @@ fun (module C : CONNECTION) ->
      C.exec Q.at_delete C.Param.([|int32 ak.ak_id|])
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
      C.find_opt Q.et_id_of_name
		 C.Tuple.(int32 0) C.Param.([|text name|])

    let name, name_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      C.find Q.et_name_of_id C.Tuple.(text 0) C.Param.([|int32 et|])

    let create etn =
      with_db @@ fun (module C) ->
      C.find Q.et_create C.Tuple.(int32 0) C.Param.([|text etn|])

    let delete et =
      with_db @@ fun (module C) ->
      C.exec Q.et_delete C.Param.([|int32 et|])

    let all () =
      with_db @@ fun (module C) ->
      C.fold Q.et_all (fun tup -> Set.add (C.Tuple.int32 0 tup)) [||]
	     Set.empty

    let entity_name_tmpl, entity_name_tmpl_cache = memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      C.find Q.et_entity_name_tmpl C.Tuple.(text 0) C.Param.([|int32 et|])

    let set_entity_name_tmpl et name =
      with_db @@ fun (module C) ->
      C.exec Q.et_set_entity_name_tmpl C.Param.([|text name; int32 et|])

    let can_dsub, can_dsub_cache =
      memo_2lwt @@ fun (et0, et1) ->
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.find_opt Q.et_can_dsub C.Tuple.(fun tup -> mult 0 tup, mult 1 tup)
		 C.Param.([|int32 et0; int32 et1|])

    let dsub, dsub_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.fold Q.et_dsub
	     C.Tuple.(fun tup -> Map.add (int32 0 tup) (mult 1 tup, mult 2 tup))
	     C.Param.([|int32 et|])
	     Map.empty

    let dsuper, dsuper_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.fold Q.et_dsuper
	     C.Tuple.(fun tup -> Map.add (int32 0 tup) (mult 1 tup, mult 2 tup))
	     C.Param.([|int32 et|])
	     Map.empty

    let dsub_elements () =
      with_db @@ fun (module C) ->
      let mult i tup = Multiplicity.of_int (C.Tuple.int i tup) in
      C.fold Q.et_dsub_elements
	     C.Tuple.(fun tup acc -> (int32 0 tup, int32 1 tup,
				      mult 2 tup, mult 3 tup) :: acc)
	     [||] []

    let allow_dsub mu0 mu1 et0 et1 =
      with_db @@ fun (module C) ->
      let mu0, mu1 = Multiplicity.(to_int mu0, to_int mu1) in
      C.exec Q.et_allow_dsub
	     C.Param.([|int mu0; int mu1; int32 et0; int32 et1|])

    let disallow_dsub et0 et1 =
      with_db @@ fun (module C) ->
      C.exec Q.et_disallow_dsub C.Param.([|int32 et0; int32 et1|])

    let can_asub_byattr, can_asub_byattr_cache =
      memo_2lwt @@ fun (et, et') ->
      with_db @@ fun ((module C) as conn) ->
      let aux tup akm =
	lwt ak = Attribute_type.of_id' ~conn (C.Tuple.int32 0 tup) in
	let mult = Multiplicity.of_int (C.Tuple.int 1 tup) in
	Lwt.return (Attribute_type.Map.add ak mult akm) in
      C.fold_s Q.et_can_asub_byattr aux C.Param.([|int32 et; int32 et'|])
	       Attribute_type.Map.empty

    let can_asub', can_asub_cache =
      memo_3lwt @@ fun (et, et', ak) ->
      with_db @@ fun (module C) ->
      let aux tup = Multiplicity.of_int (C.Tuple.int 0 tup) in
      C.find_opt Q.et_can_asub aux
		 C.Param.([|int32 et; int32 et'; int32 ak|])

    let can_asub et et' ak =
      can_asub' et et' ak.Attribute_type.ak_id

    let asub_elements () =
      with_db @@ fun ((module C) as conn) ->
      let aux tup acc =
	let et0, et1 = C.Tuple.(int32 0 tup, int32 1 tup) in
	let mu = Multiplicity.of_int C.Tuple.(int 3 tup) in
	Attribute_type.of_id' ~conn C.Tuple.(int32 2 tup) >|= fun ak ->
	(et0, et1, ak, mu) :: acc in
      C.fold_s Q.et_asub_elements aux [||] []

    let allow_asub et et' (Attribute_type.Ex ak) mu =
      with_db @@ fun (module C) ->
      let mu = Multiplicity.to_int mu in
      C.exec Q.et_allow_asub
	     C.Param.([|int32 et; int32 et'; int32 ak.Attribute_type.ak_id;
			int mu|])

    let disallow_asub et et' (Attribute_type.Ex ak) =
      with_db @@ fun (module C) ->
      C.exec Q.et_disallow_asub
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

    let top_id = 1l
    let top = of_id top_id

    let type_, type_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      C.find Q.e_type C.Tuple.(int32 0) C.Param.([|int32 e|])

    let rank, rank_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      C.find Q.e_rank C.Tuple.(int 0) C.Param.([|int32 e|])

    let access_opt, access_opt_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      C.find Q.e_access C.Tuple.(option int32 0) C.Param.([|int32 e|])

    let rec access e =
      match_lwt access_opt e with
      | Some e' -> Lwt.return e'
      | None -> assert (e <> top_id); access top_id

    let type_members, type_members_cache = memo_1lwt @@ fun entity_type_id ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_type_members add C.Param.([|int32 entity_type_id|]) Set.empty

    let minimums, minimums_cache = memo_0lwt @@ fun () ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_minimums add [||] Set.empty

    let dsub, dsub_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_dsub add C.Param.([|int32 e|]) Set.empty

    let dsuper, dsuper_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_dsuper add C.Param.([|int32 e|]) Set.empty

    let create ?access entity_type =
      with_db @@ fun (module C) ->
	C.find Q.e_create_entity C.Tuple.(int32 0)
	       C.Param.([|int32 entity_type; option int32 access|])

    let modify ?access e =
      with_db @@ fun (module C) ->
      Pwt_option.iter_s
	(fun access ->
	  C.exec Q.e_set_entity_access C.Param.([|int32 access; int32 e|]) >>
	  Lwt.return (Cache.remove access_opt_cache e))
	access

    let delete e =
      with_db @@ fun (module C) ->
      C.exec Q.e_delete_entity C.Param.([|int32 e|]) >|= fun () ->
      Cache.clear minimums_cache;
      Cache.clear dsub_cache

    let is_sub subentity superentity =
      if subentity = superentity then Lwt.return_true else
      let k = subentity, superentity in
      try Lwt.return (Cache.find inclusion_cache k)
      with Not_found ->
	lwt r_lim = rank superentity in
	lwt c = with_db @@ fun (module C) ->
	  C.find_opt Q.e_select_precedes (fun _ -> ())
		     C.Param.([|int r_lim; int32 subentity;
				int r_lim; int32 superentity|]) >|=
	  function None -> false | Some () -> true in
	Cache.replace inclusion_cache preceq_grade k c;
	Lwt.return c

    let clear_inclusion_caches () =
      Cache.clear minimums_cache;
      Cache.clear dsub_cache;
      Cache.clear dsuper_cache;
      Cache.clear inclusion_cache

    type ptuple =
      Ptuple : (module Caqti_sigs.TUPLE with type t = 't) * 't -> ptuple

    let getattr_integer, getattr_integer_cache =
      memo_3lwt @@ fun (e, e', ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_integer_attribution
	     C.Tuple.(fun tup -> Values.add (int 0 tup))
	     C.Param.([|int32 e; int32 e'; int32 ak_id|])
	     (Values.empty Type.Int)

    let getattr_text, getattr_text_cache =
      memo_3lwt @@ fun (e, e', ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_text_attribution
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

    let asub_eq_integer, asub_eq_integer_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub_eq_integer f C.Param.([|int32 e; int32 ak_id; int x|])
	     Set.empty

    let asub_eq_text, asub_eq_text_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub_eq_text f C.Param.([|int32 e; int32 ak_id; text x|])
	     Set.empty

    let asub_eq (type a) e (ak : a Attribute_type.t1) : a -> Set.t Lwt.t =
      let open Attribute_type in
      match type1 ak with
      | Type.Bool -> fun x -> asub_eq_integer e ak.ak_id (if x then 1 else 0)
      | Type.Int -> asub_eq_integer e ak.ak_id
      | Type.String -> asub_eq_text e ak.ak_id

    let asuper_eq_integer, asuper_eq_integer_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper_eq_integer f C.Param.([|int32 e; int32 ak_id; int x|])
	     Set.empty

    let asuper_eq_text, asuper_eq_text_cache =
      memo_3lwt @@ fun (e, ak_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper_eq_text f C.Param.([|int32 e; int32 ak_id; text x|])
	     Set.empty

    let asuper_eq (type a) e (ak : a Attribute_type.t1) : a -> Set.t Lwt.t =
      let open Attribute_type in
      match type1 ak with
      | Type.Bool -> fun x -> asuper_eq_integer e ak.ak_id (if x then 1 else 0)
      | Type.Int -> asuper_eq_integer e ak.ak_id
      | Type.String -> asuper_eq_text e ak.ak_id

    let asub_get_integer, asub_get_integer_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, int 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asub_get_integer f C.Param.([|int32 e; int32 ak_id|])
	     Map.empty

    let asub_get_text, asub_get_text_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, text 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asub_get_text f C.Param.([|int32 e; int32 ak_id|]) Map.empty

    let asub_get (type a) e (ak : a Attribute_type.t1)
	  : a Values.t Map.t Lwt.t =
      match Attribute_type.type1 ak with
      | Type.Bool ->
	lwt m = asub_get_integer e ak.Attribute_type.ak_id in
	let t = Attribute_type.type1 ak in
	let aux vs = Values.fold (Values.add *< ((<>) 0)) vs (Values.empty t) in
	Lwt.return (Map.map aux m)
      | Type.Int -> asub_get_integer e ak.Attribute_type.ak_id
      | Type.String -> asub_get_text e ak.Attribute_type.ak_id

    let asuper_get_integer, asuper_get_integer_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, int 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asuper_get_integer f C.Param.([|int32 e; int32 ak_id|])
	     Map.empty

    let asuper_get_text, asuper_get_text_cache =
      memo_2lwt @@ fun (e, ak_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, text 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asuper_get_text f C.Param.([|int32 e; int32 ak_id|]) Map.empty

    let asuper_get (type a) e (ak : a Attribute_type.t1)
	  : a Values.t Map.t Lwt.t =
      match Attribute_type.type1 ak with
      | Type.Bool ->
	lwt m = asuper_get_integer e ak.Attribute_type.ak_id in
	let t = Attribute_type.type1 ak in
	let aux vs = Values.fold (Values.add *< ((<>) 0)) vs (Values.empty t) in
	Lwt.return (Map.map aux m)
      | Type.Int -> asuper_get_integer e ak.Attribute_type.ak_id
      | Type.String -> asuper_get_text e ak.Attribute_type.ak_id

    let clear_integer_caches () =
      Cache.clear getattr_integer_cache;
      Cache.clear asub_eq_integer_cache;
      Cache.clear asuper_eq_integer_cache;
      Cache.clear asub_get_integer_cache;
      Cache.clear asuper_get_integer_cache

    let clear_text_caches () =
      Cache.clear getattr_text_cache;
      Cache.clear asub_eq_text_cache;
      Cache.clear asuper_eq_text_cache;
      Cache.clear asub_get_text_cache;
      Cache.clear asuper_get_text_cache

    let clear_attr_caches (type a) (ak : a Attribute_type.t1) : unit =
      match Attribute_type.type1 ak with
      | Type.Bool -> clear_integer_caches ()
      | Type.Int -> clear_integer_caches ()
      | Type.String -> clear_text_caches ()

    (* Modifying Functions *)

    let set_rank r e =
      with_db (fun (module C) ->
		C.exec Q.e_set_entity_rank C.Param.[|int r; int32 e|])
	>|= fun () -> Cache.replace rank_cache fetch_grade e r

    let rec raise_rank r_min e =
      lwt r = rank e in
      if r >= r_min then Lwt.return_unit else
      begin
	dsub e >>= Set.iter_s (raise_rank (r_min + 1)) >>
	set_rank r_min e
      end

    let rec lower_rank e =
      lwt r = rank e in
      let update_rank eS r' =
	if r' = r then Lwt.return r' else
	rank eS >|= max r' *< succ in
      lwt esS = dsuper e in
      lwt r' = Set.fold_s update_rank esS 0 in
      if r' = r then Lwt.return_unit else begin
	set_rank r' e >>
	dsub e >>= Set.iter_s begin fun eP ->
	  lwt rP = rank eP in
	  if rP = r + 1 then lower_rank eP
			else Lwt.return_unit
	end
      end

    let force_dsub' subentity superentity (module C : CONNECTION) =
      C.exec Q.e_maybe_insert_inclusion
	C.Param.([|int32 subentity; int32 superentity;
		   int32 subentity; int32 superentity|]) >|=
      fun () ->
	clear_inclusion_caches ();
	emit_changed subentity `Dsuper;
	emit_changed superentity `Dsub

    let relax_dsub' subentity superentity (module C : CONNECTION) =
      C.exec Q.e_delete_inclusion
	C.Param.([|int32 subentity; int32 superentity|]) >|=
      fun () ->
	clear_inclusion_caches ();
	emit_changed subentity `Dsuper;
	emit_changed superentity `Dsub

    let force_dsub subentity superentity =
      lwt is_super = is_sub superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      lwt subentity_rank = rank subentity in
      lwt superentity_rank = rank superentity in
      raise_rank (max subentity_rank (superentity_rank + 1)) subentity >>
      with_db (force_dsub' subentity superentity)
      (* TODO: Update is_subsumed. *)

    let relax_dsub subentity superentity =
      (* TODO: Update is_subsumed. *)
      with_db (relax_dsub' subentity superentity) >>
      lwt subentity_rank = rank subentity in
      lwt superentity_rank = rank superentity in
      if subentity_rank > superentity_rank + 1 then Lwt.return_unit
					       else lower_rank subentity

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
      | Type.Bool -> aux Q.e_insert_integer_attribution
		     (fun x -> C.Param.int (if x then 1 else 0))
      | Type.Int -> aux Q.e_insert_integer_attribution C.Param.int
      | Type.String -> aux Q.e_insert_text_attribution C.Param.text

    let check_mult e e' ak =
      lwt et = type_ e in
      lwt et' = type_ e' in
      match_lwt Entity_type.can_asub et et' ak with
      | None ->
	lwt etn = Entity_type.name et in
	lwt etn' = Entity_type.name et' in
	lwt_failure_f "addattr: %s is not allowed from %s to %s."
		      ak.Attribute_type.ak_name etn etn'
      | Some mu -> Lwt.return mu

    let addattr (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      lwt xs_pres = getattr e e' ak in
      lwt xs =
	match_lwt check_mult e e' ak with
	| Multiplicity.May1 | Multiplicity.Must1 ->
	  if Values.is_empty xs_pres
	  then Lwt.return xs
	  else lwt_failure_f "addattr: Attribute already set.";
	| Multiplicity.May | Multiplicity.Must ->
	  let ht = Hashtbl.create 7 in
	  Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
	  Lwt.return @@
	    List.filter
	      (fun x -> if Hashtbl.mem ht x then false else
			(Hashtbl.add ht x (); true)) xs in
      if xs = [] then Lwt.return_unit else
      addattr' e e' ak xs >|=
      fun () ->
	clear_attr_caches ak;
	emit_changed e `Asuper;
	emit_changed e' `Asub

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
      | Type.Bool -> aux Q.e_delete_integer_attribution
		     (fun x -> C.Param.int (if x then 1 else 0))
      | Type.Int -> aux Q.e_delete_integer_attribution C.Param.int
      | Type.String -> aux Q.e_delete_text_attribution C.Param.text

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
	emit_changed e `Asuper;
	emit_changed e' `Asub

    let setattr (type a) e e' (ak : a Attribute_type.t1) (xs : a list) =
      begin match_lwt check_mult e e' ak with
      | Multiplicity.May1 | Multiplicity.Must1 ->
	if List.length xs <= 1 then Lwt.return_unit else
	lwt_failure_f "addattr: Attribute already set.";
      | Multiplicity.May | Multiplicity.Must ->
	Lwt.return_unit
      end >>
      lwt xs_pres = getattr e e' ak in
      let ht = Hashtbl.create 7 in
      Values.iter (fun x -> Hashtbl.add ht x false) xs_pres;
      let xs_ins =
	List.filter
	  (fun x ->
	    let pres = Hashtbl.mem ht x in
	    Hashtbl.replace ht x true;
	    not pres)
	  xs in
      let xs_del =
	Hashtbl.fold (fun x keep acc -> if keep then acc else x :: acc) ht [] in
      (if xs_del = [] then Lwt.return_unit else delattr' e e' ak xs_del) >>
      (if xs_ins = [] then Lwt.return_unit else addattr' e e' ak xs_ins) >|=
      fun () ->
	clear_attr_caches ak;
	emit_changed e `Asuper;
	emit_changed e' `Asub

  end

  let entity_changed = Int32_event_table.event Entity.changed_event_table

end : S)

let connect uri = make (`Uri uri)
