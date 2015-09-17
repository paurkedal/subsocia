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
open Unprime_char
open Unprime_list
open Unprime_option

let fetch_grade = 1e-3 *. cache_second
let attribute_type_grade = fetch_grade
let entity_type_grade = fetch_grade
let entity_grade = fetch_grade
let preceq_grade = 1e-2 *. cache_second (* TODO: Highly non-constant. *)
let attribution_grade = fetch_grade

let schema_prefix = ref "subsocia."

let int_of_bool x = if x then 1 else 0

let format_query sql = Caqti_query.prepare_fun @@ fun lang ->
  let buf = Buffer.create (String.length sql) in
  let n = String.length sql in
  begin match lang with
  | `Pgsql ->
    let p = ref 0 in
    for i = 0 to n - 1 do
      match sql.[i] with
      | '?' -> incr p; Printf.bprintf buf "$%d" !p
      | '@' when i + 1 < n && Char.is_alpha sql.[i + 1] ->
	Buffer.add_string buf !schema_prefix
      | ch -> Buffer.add_char buf ch
    done
  | `Sqlite ->
    for i = 0 to n - 1 do
      match sql.[i] with
      | '@' when i + 1 < n && Char.is_alpha sql.[i + 1] -> ()
      | ch -> Buffer.add_char buf ch
    done
  | _ -> raise Caqti_query.Missing_query_string
  end;
  Buffer.contents buf

let format_query_f fmt = Printf.ksprintf format_query fmt

(* TODO: Read custom mapping from a configuration file. These are only the
 * currently shipped catalogs. *)
let tsconfig_of_lang2 = function
  | "da" -> "danish"
  | "en" -> "english"
  | "fi" -> "finish"
  | "fr" -> "frensh"
  | "de" -> "german"
  | "hu" -> "hungarian"
  | "it" -> "italian"
  | "nb" | "nn" | "no" -> "norwegian"
  | "nl" -> "dutch"
  | "pt" -> "portugese"
  | "ro" -> "romanian"
  | "ru" -> "russian"
  | "es" -> "spanish"
  | "sv" -> "swedish"
  | "tr" -> "turkish"
  | _ -> "simple"

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

  (* Attribute Type *)

  let at_by_id =
    q "SELECT attribute_name, value_type FROM @attribute_type \
       WHERE attribute_type_id = ?"
  let at_by_name =
    q "SELECT attribute_type_id, value_type FROM @attribute_type \
       WHERE attribute_name = ?"
  let at_create =
    q "INSERT INTO @attribute_type (attribute_name, value_type, fts_config) \
       VALUES (?, ?, ?) RETURNING attribute_type_id"
  let at_delete =
    q "DELETE FROM @attribute_type WHERE attribute_type_id = ?"

  (* Attribute Uniqueness *)

  let au_affected =
    q "SELECT attribute_type_id FROM @attribute_uniqueness \
       WHERE attribute_uniqueness_id = ? ORDER BY attribute_type_id"
  let au_affecting =
    q "SELECT attribute_uniqueness_id FROM @attribute_uniqueness \
       WHERE attribute_type_id = ?"
  let au_force =
    let ht = Hashtbl.create 7 in
    fun n ->
      assert (n > 0);
      try Hashtbl.find ht n with
      | Not_found ->
	let buf = Buffer.create 512 in
	Buffer.add_string buf
	  "INSERT INTO @attribute_uniqueness \
	   SELECT attribute_uniqueness_id, attribute_type_id \
	   FROM (SELECT nextval('subsocia.attribute_uniqueness_id_seq')) \
		  AS seq(attribute_uniqueness_id) \
		CROSS JOIN (VALUES (?::int)";
	for i = 1 to n - 1 do Buffer.add_string buf ", (?::int)" done;
	Buffer.add_string buf
	  ") AS ats(attribute_type_id) RETURNING attribute_type_id";
	let query = q (Buffer.contents buf) in
	Hashtbl.add ht n query;
	query
  let au_relax =
    q "DELETE FROM @attribute_uniqueness WHERE attribute_uniqueness_id = ?"

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
    q "SELECT dsub_mult, dsuper_mult \
       FROM @inclusion_type \
       WHERE dsub_type_id = ? AND dsuper_type_id = ?"
  let et_dsub =
    q "SELECT dsub_type_id, dsub_mult, dsuper_mult \
       FROM @inclusion_type WHERE dsuper_type_id = ?"
  let et_dsuper =
    q "SELECT dsuper_type_id, dsub_mult, dsuper_mult \
       FROM @inclusion_type WHERE dsub_type_id = ?"
  let et_dsub_elements =
    q "SELECT dsub_type_id, dsuper_type_id, dsub_mult, dsuper_mult \
       FROM @inclusion_type"
  let et_allow_dsub =
    q "INSERT INTO @inclusion_type \
	(dsub_mult, dsuper_mult, dsub_type_id, dsuper_type_id) \
       VALUES (?, ?, ?, ?)"
  let et_disallow_dsub =
    q "DELETE FROM @inclusion_type \
       WHERE dsub_type_id = ? AND dsuper_type_id = ?"

  let et_can_asub_byattr =
    q "SELECT attribute_type_id, attribute_mult \
       FROM @attribution_type \
       WHERE asub_type_id = ? AND asuper_type_id = ?"
  let et_can_asub =
    q "SELECT attribute_mult FROM @attribution_type \
       WHERE asub_type_id = ? AND asuper_type_id = ? \
	 AND attribute_type_id = ?"
  let et_asub_elements =
    q "SELECT asub_type_id, asuper_type_id, attribute_type_id, attribute_mult \
       FROM @attribution_type"
  let et_allow_asub =
    q "INSERT INTO @attribution_type \
	(asub_type_id, asuper_type_id, attribute_type_id, attribute_mult) \
       VALUES (?, ?, ?, ?)"
  let et_disallow_asub =
    q "DELETE FROM @attribution_type \
       WHERE asub_type_id = ? AND asuper_type_id = ? AND attribute_type_id = ?"

  (* Entites *)

  let e_dsuper_any =
    q "SELECT dsuper_id FROM @inclusion WHERE dsub_id = ?"
  let e_dsub_any =
    q "SELECT dsub_id FROM @inclusion WHERE dsuper_id = ?"

  let e_dsuper_typed =
    q "SELECT entity_id \
       FROM @inclusion JOIN @entity ON dsuper_id = entity_id \
       WHERE dsub_id = ? AND entity_type_id = ?"
  let e_dsub_typed =
    q "SELECT entity_id \
       FROM @inclusion JOIN @entity ON dsub_id = entity_id \
       WHERE dsuper_id = ? AND entity_type_id = ?"

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
	(SELECT 0 FROM @inclusion WHERE dsuper_id = entity_id)"

  let e_select_precedes =
    q "WITH RECURSIVE successors(entity_id) AS ( \
	  SELECT i.dsuper_id AS entity_id \
	  FROM @inclusion i \
	  JOIN @entity e ON e.entity_id = i.dsuper_id \
	  WHERE i.is_subsumed = false \
	    AND e.entity_rank >= ? \
	    AND i.dsub_id = ? \
	UNION \
	  SELECT i.dsuper_id \
	  FROM @inclusion i \
	  JOIN @entity e ON e.entity_id = i.dsuper_id \
	  JOIN successors c ON i.dsub_id = c.entity_id \
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
    q "INSERT INTO @inclusion (dsub_id, dsuper_id) SELECT ?, ? \
       WHERE NOT EXISTS (SELECT 0 FROM @inclusion \
			 WHERE dsub_id = ? AND dsuper_id = ?)"

  let e_delete_inclusion =
    q "DELETE FROM @inclusion WHERE dsub_id = ? AND dsuper_id = ?"

  (* Params: sub_id, super_id, sub_id, super_id *)
  let e_subsume_inclusion =
    q "WITH RECURSIVE \
	lb(id) AS ( \
	    SELECT ?::integer AS id \
	  UNION \
	    SELECT i.dsub_id AS id \
	    FROM lb JOIN @inclusion i ON i.dsuper_id = lb.id \
	    WHERE i.is_subsumed = false \
	), \
	ub(id) AS ( \
	    SELECT ?::integer AS id \
	  UNION \
	    SELECT i.dsuper_id AS id \
	    FROM ub JOIN @inclusion i ON i.dsub_id = ub.id \
	    WHERE i.is_subsumed = false \
	) \
      UPDATE @inclusion i SET is_subsumed = true \
      WHERE (i.dsub_id <> ?::integer OR i.dsuper_id <> ?::integer) \
	AND i.dsub_id IN (SELECT * FROM lb) \
	AND i.dsuper_id IN (SELECT * FROM ub) \
	AND i.is_subsumed = false"

  let e_is_subsumed =
    q "SELECT is_subsumed FROM @inclusion WHERE dsub_id = ? AND dsuper_id = ?"

  (* Params: sub_id, super_id, sub_id, super_id *)
  let e_unsubsume_inclusion =
    q "WITH RECURSIVE \
	lb(id) AS ( \
	    SELECT ?::integer AS id \
	  UNION \
	    SELECT i.dsub_id AS id \
	    FROM lb JOIN @inclusion i ON i.dsuper_id = lb.id \
	    WHERE is_subsumed = false \
	), \
	ub(id) AS ( \
	    SELECT ?::integer AS id \
	  UNION \
	    SELECT i.dsuper_id AS id \
	    FROM ub JOIN @inclusion i ON i.dsub_id = ub.id \
	    WHERE is_subsumed = false \
	) \
      UPDATE @inclusion i SET is_subsumed = false \
	WHERE i.is_subsumed = true \
	  AND i.dsub_id IN (SELECT * FROM lb) \
	  AND i.dsuper_id IN (SELECT * FROM ub) \
	  AND NOT EXISTS ( \
	    WITH RECURSIVE acc(id) AS ( \
		SELECT i.dsub_id AS id \
	      UNION \
		SELECT j.dsuper_id AS id \
		FROM @inclusion j JOIN acc ON j.dsub_id = acc.id \
		WHERE (j.dsub_id <> ?::integer OR j.dsuper_id <> ?::integer) \
		  AND (j.dsub_id <> i.dsub_id OR j.dsuper_id <> i.dsuper_id) \
	    ) \
	    SELECT 1 FROM acc WHERE acc.id = i.dsuper_id \
	  )"

  let e_select_attribution_bool =
    q "SELECT value FROM @attribution_bool \
       WHERE asub_id = ? AND asuper_id = ? AND attribute_type_id = ?"
  let e_select_attribution_int =
    q "SELECT value FROM @attribution_int \
       WHERE asub_id = ? AND asuper_id = ? AND attribute_type_id = ?"
  let e_select_attribution_string =
    q "SELECT value FROM @attribution_string \
       WHERE asub_id = ? AND asuper_id = ? AND attribute_type_id = ?"

  let e_insert_attribution_bool =
    q "INSERT INTO @attribution_bool \
	(asub_id, asuper_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"
  let e_insert_attribution_int =
    q "INSERT INTO @attribution_int \
	(asub_id, asuper_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"
  let e_insert_attribution_string =
    q "INSERT INTO @attribution_string \
	(asub_id, asuper_id, attribute_type_id, value) \
       VALUES (?, ?, ?, ?)"

  let e_delete_attribution_bool =
    q "DELETE FROM @attribution_bool \
       WHERE asub_id = ? AND asuper_id = ? \
	 AND attribute_type_id = ? AND value = ?"
  let e_delete_attribution_int =
    q "DELETE FROM @attribution_int \
       WHERE asub_id = ? AND asuper_id = ? \
	 AND attribute_type_id = ? AND value = ?"
  let e_delete_attribution_string =
    q "DELETE FROM @attribution_string \
       WHERE asub_id = ? AND asuper_id = ? \
	 AND attribute_type_id = ? AND value = ?"

  let ap1_ops = [|"="; "<="; ">="|]
  let ap1_eq = 0
  let ap1_leq = 1
  let ap1_geq = 2

  let e_asub_present_bool =
    q("SELECT asub_id FROM @attribution_bool \
       WHERE asuper_id = ? AND attribute_type_id = ?")
  let e_asub_present_int =
    q("SELECT asub_id FROM @attribution_int \
       WHERE asuper_id = ? AND attribute_type_id = ?")
  let e_asub_present_string =
    q("SELECT asub_id FROM @attribution_string \
       WHERE asuper_id = ? AND attribute_type_id = ?")

  let e_asuper_present_bool =
    q("SELECT asuper_id FROM @attribution_bool \
       WHERE asub_id = ? AND attribute_type_id = ?")
  let e_asuper_present_int =
    q("SELECT asuper_id FROM @attribution_int \
       WHERE asub_id = ? AND attribute_type_id = ?")
  let e_asuper_present_string =
    q("SELECT asuper_id FROM @attribution_string \
       WHERE asub_id = ? AND attribute_type_id = ?")

  let e_asub1_bool = ap1_ops |> Array.map @@ fun op ->
    q("SELECT asub_id FROM @attribution_bool \
       WHERE asuper_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asub1_int = ap1_ops |> Array.map @@ fun op ->
    q("SELECT asub_id FROM @attribution_int \
       WHERE asuper_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asub1_string = ap1_ops |> Array.map @@ fun op ->
    q("SELECT asub_id FROM @attribution_string \
       WHERE asuper_id = ? AND attribute_type_id = ? AND value "^op^" ?")

  let e_asuper1_bool = ap1_ops |> Array.map @@ fun op ->
    q("SELECT asuper_id FROM @attribution_bool \
       WHERE asub_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asuper1_int = ap1_ops |> Array.map @@ fun op ->
    q("SELECT asuper_id FROM @attribution_int \
       WHERE asub_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asuper1_string = ap1_ops |> Array.map @@ fun op ->
    q("SELECT asuper_id FROM @attribution_string \
       WHERE asub_id = ? AND attribute_type_id = ? AND value "^op^" ?")

  let e_asub2_between_int =
    q "SELECT asub_id FROM @attribution_int \
       WHERE asuper_id = ? AND attribute_type_id = ? \
	 AND value >= ? AND value < ?"
  let e_asub2_between_string =
    q "SELECT asub_id FROM @attribution_string \
       WHERE asuper_id = ? AND attribute_type_id = ? \
	 AND value >= ? AND value < ?"

  let e_asuper2_between_int =
    q "SELECT asuper_id FROM @attribution_int \
       WHERE asub_id = ? AND attribute_type_id = ? \
	 AND value >= ? AND value < ?"
  let e_asuper2_between_string =
    q "SELECT asuper_id FROM @attribution_string \
       WHERE asub_id = ? AND attribute_type_id = ? \
	 AND value >= ? AND value < ?"

  let e_asub1_search =
    q "SELECT asub_id FROM @attribution_string \
       WHERE asuper_id = ? AND attribute_type_id = ? \
	 AND value SIMILAR TO ?"
  let e_asuper1_search =
    q "SELECT asuper_id FROM @attribution_string \
       WHERE asub_id = ? AND attribute_type_id = ? \
	 AND value SIMILAR TO ?"

  let e_asub1_search_fts =
    q "SELECT asub_id FROM @attribution_string_fts \
       WHERE asuper_id = ? \
	 AND fts_vector @@ to_tsquery(fts_config::regconfig, ?)"
  let e_asuper1_search_fts =
    q "SELECT asuper_id FROM @attribution_string_fts \
       WHERE asub_id = ? \
	 AND fts_vector @@ to_tsquery(fts_config::regconfig, ?)"

  let _asub_fts with_et with_super with_limit =
    format_query_f
      "SELECT * FROM
	(SELECT a.asub_id, \
		ts_rank(fts_vector, to_tsquery(fts_config::regconfig, ?)) AS r \
	 FROM @attribution_string_fts AS a%s%s WHERE a.asuper_id = ?%s%s \
	 ORDER BY r DESC%s) AS sq
       WHERE r > ?"
      (if with_et then " JOIN @entity ON a.asub_id = entity_id" else "")
      (if with_super then " JOIN @transitive_reflexive_inclusion AS c \
			      ON c.subentity_id = a.asub_id" else "")
      (if with_et then " AND entity_type_id = ?" else "")
      (if with_super then " AND c.superentity_id = ?" else "")
      (if with_limit then " LIMIT ?" else "")

  let _asuper_fts with_et with_super with_limit =
    format_query_f
      "SELECT * FROM
	(SELECT a.asuper_id, \
		ts_rank(fts_vector, to_tsquery(fts_config::regconfig, ?)) AS r \
	 FROM @attribution_string_fts AS a%s%s WHERE a.asub_id = ?%s%s \
	 ORDER BY r DESC%s) AS sq
       WHERE r > ?"
      (if with_et then " JOIN @entity ON a.asuper_id = entity_id" else "")
      (if with_super then " JOIN @transitive_reflexive_inclusion AS c \
			      ON c.subentity_id = a.asuper_id" else "")
      (if with_et then " AND entity_type_id = ?" else "")
      (if with_super then " AND c.superentity_id = ?" else "")
      (if with_limit then " LIMIT ?" else "")

  let e_asub_fts		= _asub_fts   false false false
  let e_asuper_fts		= _asuper_fts false false false
  let e_asub_fts_limit		= _asub_fts   false false true
  let e_asuper_fts_limit	= _asuper_fts false false true
  let e_asub_fts_super		= _asub_fts   false true  false
  let e_asuper_fts_super	= _asuper_fts false true  false
  let e_asub_fts_super_limit	= _asub_fts   false true  true
  let e_asuper_fts_super_limit	= _asuper_fts false true  true
  let e_asub_fts_et		= _asub_fts   true  false false
  let e_asuper_fts_et		= _asuper_fts true  false false
  let e_asub_fts_et_limit	= _asub_fts   true  false true
  let e_asuper_fts_et_limit	= _asuper_fts true  false true
  let e_asub_fts_et_super	= _asub_fts   true  true  false
  let e_asuper_fts_super_et	= _asuper_fts true  true  false
  let e_asub_fts_et_super_limit	= _asub_fts   true  true  true
  let e_asuper_fts_et_super_limit=_asuper_fts true  true  true

  let e_asub_get_bool =
    q "SELECT asub_id, value FROM @attribution_bool \
       WHERE asuper_id = ? AND attribute_type_id = ?"
  let e_asub_get_int =
    q "SELECT asub_id, value FROM @attribution_int \
       WHERE asuper_id = ? AND attribute_type_id = ?"
  let e_asub_get_string =
    q "SELECT asub_id, value FROM @attribution_string \
       WHERE asuper_id = ? AND attribute_type_id = ?"

  let e_asuper_get_bool =
    q "SELECT asuper_id, value FROM @attribution_bool \
       WHERE asub_id = ? AND attribute_type_id = ?"
  let e_asuper_get_int =
    q "SELECT asuper_id, value FROM @attribution_int \
       WHERE asub_id = ? AND attribute_type_id = ?"
  let e_asuper_get_string =
    q "SELECT asuper_id, value FROM @attribution_string \
       WHERE asub_id = ? AND attribute_type_id = ?"

  let fts_clear =
    q "DELETE FROM subsocia.attribution_string_fts \
       WHERE asub_id = ? AND asuper_id = ?"
  let fts_insert =
    q "INSERT INTO subsocia.attribution_string_fts \
       SELECT a.asub_id, a.asuper_id, at.fts_config, \
	      to_tsvector(at.fts_config::regconfig, string_agg(value, '$')) \
       FROM subsocia.attribution_string AS a \
	 NATURAL JOIN subsocia.attribute_type AS at \
       WHERE NOT at.fts_config IS NULL \
	 AND a.asub_id = ? AND a.asuper_id = ? \
       GROUP BY a.asub_id, a.asuper_id, at.fts_config"
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
let memo_4lwt f = let g, c = memo_1lwt f in
		  (fun x0 x1 x2 x3 -> g (x0, x1, x2, x3)), c
let memo_5lwt f = let g, c = memo_1lwt f in
		  (fun x0 x1 x2 x3 x4 -> g (x0, x1, x2, x3, x4)), c
let memo_6lwt f = let g, c = memo_1lwt f in
		  (fun x0 x1 x2 x3 x4 x5 -> g (x0, x1, x2, x3, x4, x5)), c

module B = struct
  module Attribute_type = struct
    type 'a t = {
      at_id : int32;
      at_name : string;
      at_value_type : 'a Type.t;
      at_beacon : Beacon.t;
    }
    type ex = Ex : 'a t -> ex

    let dummy = {
      at_id = Int32.zero;
      at_name = "";
      at_value_type = Type.Bool;
      at_beacon = Beacon.dummy;
    }

    let value_type at = at.at_value_type

    let assert_coerce : type a. a Type.t -> ex -> a t = fun vt (Ex at) ->
      match vt, at.at_value_type with
      | Type.Bool, Type.Bool -> at
      | Type.Int, Type.Int -> at
      | Type.String, Type.String -> at
      | _, _ -> assert false

    module Comparable = struct
      type t = ex
      let compare (Ex x) (Ex y) = compare x.at_id y.at_id
    end
    module Map = Prime_enummap.Make_monadic (Comparable) (Lwt)
    module Set = Prime_enumset.Make_monadic (Comparable) (Lwt)

    (**/**)
    type t0 = ex
    type 'a t1 = 'a t
    let type0 (Ex at) = Type.Ex at.at_value_type
    let type1 = value_type
  end

  module Attribute_uniqueness = struct
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    exception Not_unique of Set.t
  end

  module Attribute = struct
    type ex = Ex : 'a Attribute_type.t * 'a -> ex
    type predicate =
      | Present : 'a Attribute_type.t -> predicate
      | Eq : 'a Attribute_type.t * 'a -> predicate
      | In : 'a Attribute_type.t * 'a Values.t -> predicate
      | Leq : 'a Attribute_type.t * 'a -> predicate
      | Geq : 'a Attribute_type.t * 'a -> predicate
      | Between : 'a Attribute_type.t * 'a * 'a -> predicate
      | Search : string Attribute_type.t * string -> predicate
      | Search_fts : string -> predicate

    (**/**)
    type t0 = ex
  end

  module Entity_type = struct
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    let compare = Int32.compare
  end

  module Entity = struct
    type t = int32

    module Set = Int32_set
    module Map = Int32_map
  end
end

module type Param = sig
 val with_db : transaction: bool ->
	       ((module Caqti_lwt.CONNECTION) -> 'a Lwt.t) -> 'a Lwt.t
end

module Make (P : Param) = struct
  let inclusion_cache = Cache.create ~cache_metric 61

  let with_db ?conn ?(transaction = false) f =
    match conn with
    | None -> P.with_db ~transaction f
    | Some conn -> f conn

  module Attribute_type = struct
    open B.Attribute_type

    let id (Ex at) = at.at_id
    let id' at = at.at_id
    let name (Ex at) = Lwt.return at.at_name
    let name' at = Lwt.return at.at_name

    let of_id', of_id_cache = Cache.memo_lwt_conn @@ fun ?conn at_id ->
      with_db ?conn @@ fun (module C : CONNECTION) ->
      C.find Q.at_by_id
	     C.Tuple.(fun tup -> string 0 tup, string 1 tup)
	     C.Param.([|int32 at_id|]) >|= fun (at_name, value_type) ->
      let Type.Ex at_value_type = Type.of_string value_type in
      Beacon.embed attribute_type_grade @@ fun at_beacon ->
      Ex {at_id; at_name; at_value_type; at_beacon}

    let of_id id = of_id' id

    let of_name, of_name_cache = memo_1lwt @@ fun at_name ->
      with_db @@ fun (module C : CONNECTION) ->
      C.find_opt Q.at_by_name
		 C.Tuple.(fun tup -> int32 0 tup, string 1 tup)
		 C.Param.([|string at_name|]) >|=
      Option.map begin fun (at_id, value_type) ->
	let Type.Ex at_value_type = Type.of_string value_type in
	Beacon.embed attribute_type_grade @@ fun at_beacon ->
	  (Ex {at_id; at_name; at_value_type; at_beacon})
      end

    let create' : type a. a Type.t -> string -> a t Lwt.t = fun vt at_name ->
      let fts =
	match vt with
	| Type.String ->
	  let len = String.length at_name in
	  if len < 3 || at_name.[len - 3] <> '.' then Some "simple" else
	  Some (tsconfig_of_lang2 (String.sub at_name (len - 2) 2))
	| _ -> None in
      with_db @@ fun ((module C : CONNECTION) as conn) ->
      C.find Q.at_create C.Tuple.(int32 0)
	     C.Param.([|string at_name; string (Type.to_string vt);
			option string fts|])
	>>= of_id' ~conn >|= assert_coerce vt

    let create (Type.Ex vt) name = create' vt name >|= fun at -> Ex at

    let delete' at =
      with_db @@ fun (module C : CONNECTION) ->
      C.exec Q.at_delete C.Param.([|int32 at.at_id|])

    (**/**)
    let delete (Ex at) = delete' at
  end

  module Attribute_uniqueness = struct
    open B.Attribute_uniqueness

    let of_id = Lwt.return
    let id au = au

    let affecting', affecting_cache = memo_1lwt @@ fun at_id ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.au_affecting C.Tuple.(fun t -> Set.add (int32 0 t))
	     C.Param.[|int32 at_id|] Set.empty

    let affecting at = affecting' (Attribute_type.id' at)

    let affected, affected_cache = memo_1lwt @@ fun au ->
      begin
	with_db @@ fun (module C : CONNECTION) ->
	C.fold Q.au_affected C.Tuple.(fun t -> List.push (int32 0 t))
	       C.Param.[|int32 au|] []
      end >>= Lwt_list.rev_map_s Attribute_type.of_id >|=
	      B.Attribute_type.Set.of_ordered_elements

    let find atset =
      let B.Attribute_type.Ex at = B.Attribute_type.Set.min_elt atset in
      affecting at >>=
      Set.filter_s (fun au -> affected au >|= B.Attribute_type.Set.equal atset)
	>|= fun auset ->
      match Set.cardinal auset with
      | 0 -> None
      | 1 -> Some (Set.min_elt auset)
      | _ -> assert false

    let force atset =
      (* TODO: Enforce non-duplication of constraints. *)
      let ats = B.Attribute_type.Set.elements atset in
      begin
	with_db @@ fun (module C : CONNECTION) ->
	C.fold (Q.au_force (B.Attribute_type.Set.cardinal atset))
	       C.Tuple.(fun t _ -> Some (int32 0 t))
	       (Array.map (C.Param.int32 *< Attribute_type.id)
			  (Array.of_list ats))
	       None
      end >|= fun au_opt ->
      assert (au_opt <> None);
      Cache.clear affecting_cache;
      Option.get au_opt

    let relax au =
      with_db @@ fun (module C : CONNECTION) ->
      C.exec Q.au_relax C.Param.[|int32 au|] >|= fun () ->
      Cache.remove affected_cache au;
      Cache.clear affecting_cache
  end

  module Attribution_sql = Subsocia_attribution_sql.Make (struct
    module Attribute_type = struct
      include B.Attribute_type
      include Attribute_type
    end
    module Attribute = B.Attribute
  end)

  module Entity_type = struct
    open B.Entity_type

    let of_id et = Lwt.return et
    let id et = et

    let of_name, of_name_cache =
      memo_1lwt @@ fun name ->
      with_db @@ fun (module C) ->
      C.find_opt Q.et_id_of_name
		 C.Tuple.(int32 0) C.Param.([|string name|])

    let name, name_cache =
      memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      C.find Q.et_name_of_id C.Tuple.(string 0) C.Param.([|int32 et|])

    let create etn =
      with_db @@ fun (module C) ->
      C.find Q.et_create C.Tuple.(int32 0) C.Param.([|string etn|])

    let delete et =
      with_db @@ fun (module C) ->
      C.exec Q.et_delete C.Param.([|int32 et|])

    let all () =
      with_db @@ fun (module C) ->
      C.fold Q.et_all (fun tup -> Set.add (C.Tuple.int32 0 tup)) [||]
	     Set.empty

    let entity_name_tmpl, entity_name_tmpl_cache = memo_1lwt @@ fun et ->
      with_db @@ fun (module C) ->
      C.find Q.et_entity_name_tmpl C.Tuple.(string 0) C.Param.([|int32 et|])

    let set_entity_name_tmpl et name =
      with_db @@ fun (module C) ->
      C.exec Q.et_set_entity_name_tmpl C.Param.([|string name; int32 et|])

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
      let aux tup at_map =
	lwt at = Attribute_type.of_id' ~conn (C.Tuple.int32 0 tup) in
	let mult = Multiplicity.of_int (C.Tuple.int 1 tup) in
	Lwt.return (B.Attribute_type.Map.add at mult at_map) in
      C.fold_s Q.et_can_asub_byattr aux C.Param.([|int32 et; int32 et'|])
	       B.Attribute_type.Map.empty

    let can_asub', can_asub_cache =
      memo_3lwt @@ fun (et, et', at) ->
      with_db @@ fun (module C) ->
      let aux tup = Multiplicity.of_int (C.Tuple.int 0 tup) in
      C.find_opt Q.et_can_asub aux
		 C.Param.([|int32 et; int32 et'; int32 at|])

    let can_asub et et' at =
      can_asub' et et' at.B.Attribute_type.at_id

    let asub_elements () =
      with_db @@ fun ((module C) as conn) ->
      let aux tup acc =
	let et0, et1 = C.Tuple.(int32 0 tup, int32 1 tup) in
	let mu = Multiplicity.of_int C.Tuple.(int 3 tup) in
	Attribute_type.of_id' ~conn C.Tuple.(int32 2 tup) >|= fun at ->
	(et0, et1, at, mu) :: acc in
      C.fold_s Q.et_asub_elements aux [||] []

    let allow_asub et et' (B.Attribute_type.Ex at) mu =
      with_db @@ fun (module C) ->
      let mu = Multiplicity.to_int mu in
      C.exec Q.et_allow_asub
	     C.Param.([|int32 et; int32 et'; int32 at.B.Attribute_type.at_id;
			int mu|])

    let disallow_asub et et' (B.Attribute_type.Ex at) =
      with_db @@ fun (module C) ->
      C.exec Q.et_disallow_asub
	     C.Param.([|int32 et; int32 et'; int32 at.B.Attribute_type.at_id|])

    let display_name ~langs ?pl = name (* FIXME *)
  end

  module Entity = struct
    open B.Entity

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

    let dsub_any, dsub_any_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_dsub_any add C.Param.([|int32 e|]) Set.empty

    let dsuper_any, dsuper_any_cache = memo_1lwt @@ fun e ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_dsuper_any add C.Param.([|int32 e|]) Set.empty

    let dsub_typed, dsub_typed_cache = memo_2lwt @@ fun (et, e) ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_dsub_typed add C.Param.([|int32 e; int32 et|]) Set.empty

    let dsuper_typed, dsuper_typed_cache = memo_2lwt @@ fun (et, e) ->
      with_db @@ fun (module C) ->
      let add tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_dsuper_typed add C.Param.([|int32 e; int32 et|]) Set.empty

    let dsub ?et e =
      match et with None -> dsub_any e | Some et -> dsub_typed et e

    let dsuper ?et e =
      match et with None -> dsuper_any e | Some et -> dsuper_typed et e

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
      Cache.clear dsub_any_cache;
      Cache.clear dsub_typed_cache

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
      Cache.clear dsub_any_cache;
      Cache.clear dsub_typed_cache;
      Cache.clear dsuper_any_cache;
      Cache.clear dsuper_typed_cache;
      Cache.clear inclusion_cache

    type ptuple =
      Ptuple : (module Caqti_sigs.TUPLE with type t = 't) * 't -> ptuple

    let getattr_bool, getattr_bool_cache =
      memo_3lwt @@ fun (e, e', at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_attribution_bool
	     C.Tuple.(fun tup -> Values.add (bool 0 tup))
	     C.Param.([|int32 e; int32 e'; int32 at_id|])
	     (Values.empty Type.Bool)

    let getattr_int, getattr_int_cache =
      memo_3lwt @@ fun (e, e', at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_attribution_int
	     C.Tuple.(fun tup -> Values.add (int 0 tup))
	     C.Param.([|int32 e; int32 e'; int32 at_id|])
	     (Values.empty Type.Int)

    let getattr_string, getattr_string_cache =
      memo_3lwt @@ fun (e, e', at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_attribution_string
	     C.Tuple.(fun tup -> Values.add (string 0 tup))
	     C.Param.([|int32 e; int32 e'; int32 at_id|])
	     (Values.empty Type.String)

    let getattr (type a) e e' (at : a B.Attribute_type.t) : a Values.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> getattr_bool e e' at.B.Attribute_type.at_id
      | Type.Int -> getattr_int e e' at.B.Attribute_type.at_id
      | Type.String -> getattr_string e e' at.B.Attribute_type.at_id

    (* TODO: Cache? *)
    let asub_conj e ps =
      with_db @@ fun (module C : CONNECTION) ->
      let q = Attribution_sql.select_asub_conj e ps in
      C.fold q (fun t -> Set.add (C.Tuple.int32 0 t)) [||] Set.empty

    (* TODO: Cache? *)
    let asuper_conj e ps =
      with_db @@ fun (module C : CONNECTION) ->
      let q = Attribution_sql.select_asuper_conj e ps in
      C.fold q (fun t -> Set.add (C.Tuple.int32 0 t)) [||] Set.empty

    let asub_present_bool, asub_present_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub_present_bool f C.Param.([|int32 e; int32 at_id|])
	     Set.empty

    let asub_present_int, asub_present_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub_present_int f C.Param.([|int32 e; int32 at_id|])
	     Set.empty

    let asub_present_string, asub_present_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub_present_string f C.Param.([|int32 e; int32 at_id|])
	     Set.empty

    let asub1_bool, asub1_bool_cache =
      memo_4lwt @@ fun (op, e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub1_bool.(op) f
	     C.Param.([|int32 e; int32 at_id; bool x|])
	     Set.empty

    let asub1_int, asub1_int_cache =
      memo_4lwt @@ fun (op, e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub1_int.(op) f
	     C.Param.([|int32 e; int32 at_id; int x|])
	     Set.empty

    let asub1_string, asub1_string_cache =
      memo_4lwt @@ fun (op, e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub1_string.(op) f
	     C.Param.([|int32 e; int32 at_id; string x|])
	     Set.empty

    let asub1 op (type a) e (at : a B.Attribute_type.t) : a -> Set.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> asub1_bool op e (Attribute_type.id' at)
      | Type.Int -> asub1_int op e (Attribute_type.id' at)
      | Type.String -> asub1_string op e (Attribute_type.id' at)

    let asub2_between_int, asub2_between_int_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub2_between_int f
	     C.Param.[|int32 e; int32 at_id; int x0; int x1|] Set.empty

    let asub2_between_string, asub2_between_string_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub2_between_string f
	     C.Param.[|int32 e; int32 at_id; string x0; string x1|] Set.empty

    let asub2_between (type a) e (at : a B.Attribute_type.t)
	: a -> a -> Set.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> fun x0 x1 ->
	if x0 = x1 then asub1_bool Q.ap1_eq e at.B.Attribute_type.at_id x0 else
	if x0 < x1 then asub_present_bool e at.B.Attribute_type.at_id else
	Lwt.return Set.empty
      | Type.Int -> asub2_between_int e at.B.Attribute_type.at_id
      | Type.String -> asub2_between_string e at.B.Attribute_type.at_id

    let asub1_search, asub1_search_cache =
      memo_3lwt @@ fun (e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub1_search f
	     C.Param.[|int32 e; int32 at_id; string x|] Set.empty

    let asub1_search_fts, asub1_search_fts_cache =
      memo_2lwt @@ fun (e, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asub1_search_fts f C.Param.[|int32 e; string x|] Set.empty

    let asub e = function
      | B.Attribute.Present at ->
	begin match B.Attribute_type.value_type at with
	| Type.Bool -> asub_present_bool e at.B.Attribute_type.at_id
	| Type.Int -> asub_present_int e at.B.Attribute_type.at_id
	| Type.String -> asub_present_string e at.B.Attribute_type.at_id
	end
      | B.Attribute.Eq (at, x) -> asub1 Q.ap1_eq e at x
      | B.Attribute.In _ as p -> asub_conj e [p]
      | B.Attribute.Leq (at, x) -> asub1 Q.ap1_leq e at x
      | B.Attribute.Geq (at, x) -> asub1 Q.ap1_geq e at x
      | B.Attribute.Between (at, x0, x1) -> asub2_between e at x0 x1
      | B.Attribute.Search (at, x) ->
	asub1_search e (Attribute_type.id' at) x
      | B.Attribute.Search_fts x -> asub1_search_fts e x

    let asub_eq at e = asub1 Q.ap1_eq at e

    let asuper_present_bool, asuper_present_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper_present_bool f C.Param.([|int32 e; int32 at_id|])
	     Set.empty

    let asuper_present_int, asuper_present_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper_present_int f C.Param.([|int32 e; int32 at_id|])
	     Set.empty

    let asuper_present_string, asuper_present_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper_present_string f C.Param.([|int32 e; int32 at_id|])
	     Set.empty

    let asuper1_bool, asuper1_bool_cache =
      memo_4lwt @@ fun (op, e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper1_bool.(op) f
	     C.Param.([|int32 e; int32 at_id; bool x|])
	     Set.empty

    let asuper1_int, asuper1_int_cache =
      memo_4lwt @@ fun (op, e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper1_int.(op) f
	     C.Param.([|int32 e; int32 at_id; int x|])
	     Set.empty

    let asuper1_string, asuper1_string_cache =
      memo_4lwt @@ fun (op, e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper1_string.(op) f
	     C.Param.([|int32 e; int32 at_id; string x|])
	     Set.empty

    let asuper1 op (type a) e (at : a B.Attribute_type.t) : a -> Set.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> asuper1_bool op e (Attribute_type.id' at)
      | Type.Int -> asuper1_int op e (Attribute_type.id' at)
      | Type.String -> asuper1_string op e (Attribute_type.id' at)

    let asuper2_between_int, asuper2_between_int_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper2_between_int f
	     C.Param.[|int32 e; int32 at_id; int x0; int x1|] Set.empty

    let asuper2_between_string, asuper2_between_string_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper2_between_string f
	     C.Param.[|int32 e; int32 at_id; string x0; string x1|] Set.empty

    let asuper2_between (type a) e (at : a B.Attribute_type.t)
	: a -> a -> Set.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> fun x0 x1 ->
	if x0 = x1 then asuper1_bool Q.ap1_eq e at.B.Attribute_type.at_id x0 else
	if x0 < x1 then asuper_present_bool e at.B.Attribute_type.at_id else
	Lwt.return Set.empty
      | Type.Int -> asuper2_between_int e at.B.Attribute_type.at_id
      | Type.String -> asuper2_between_string e at.B.Attribute_type.at_id

    let asuper1_search, asuper1_search_cache =
      memo_3lwt @@ fun (e, at_id, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper1_search f
	     C.Param.[|int32 e; int32 at_id; string x|] Set.empty

    let asuper1_search_fts, asuper1_search_fts_cache =
      memo_2lwt @@ fun (e, x) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = Set.add (C.Tuple.int32 0 tup) in
      C.fold Q.e_asuper1_search_fts f C.Param.[|int32 e; string x|] Set.empty

    let asuper_eq at e = asuper1 Q.ap1_eq at e

    let asuper e = function
      | B.Attribute.Present at ->
	begin match B.Attribute_type.value_type at with
	| Type.Bool -> asuper_present_bool e at.B.Attribute_type.at_id
	| Type.Int -> asuper_present_int e at.B.Attribute_type.at_id
	| Type.String -> asuper_present_string e at.B.Attribute_type.at_id
	end
      | B.Attribute.Eq (at, x) -> asuper1 Q.ap1_eq e at x
      | B.Attribute.In _ as p -> asuper_conj e [p]
      | B.Attribute.Leq (at, x) -> asuper1 Q.ap1_leq e at x
      | B.Attribute.Geq (at, x) -> asuper1 Q.ap1_geq e at x
      | B.Attribute.Between (at, x0, x1) -> asuper2_between e at x0 x1
      | B.Attribute.Search (at, x) ->
	asuper1_search e (Attribute_type.id' at) x
      | B.Attribute.Search_fts x -> asuper1_search_fts e x

    let asub_fts, asub_fts_cache =
      memo_6lwt @@ fun (et, super, cutoff, limit, e, fts) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = List.push C.Tuple.(int32 0 tup, float 1 tup) in
      begin match et, super, limit with
      | None, None, None ->
	C.fold Q.e_asub_fts f
	       C.Param.[|string fts; int32 e;
			 float cutoff|] []
      | None, None, Some limit ->
	C.fold Q.e_asub_fts_limit f
	       C.Param.[|string fts; int32 e; int limit;
			 float cutoff|] []
      | None, Some s, None ->
	C.fold Q.e_asub_fts_super f
	       C.Param.[|string fts; int32 e; int32 s;
			 float cutoff|] []
      | None, Some s, Some limit ->
	C.fold Q.e_asub_fts_super_limit f
	       C.Param.[|string fts; int32 e; int32 s; int limit;
			 float cutoff|] []
      | Some et, None, None ->
	C.fold Q.e_asub_fts_et f
	       C.Param.[|string fts; int32 e; int32 et;
			 float cutoff|] []
      | Some et, None, Some limit ->
	C.fold Q.e_asub_fts_et_limit f
	       C.Param.[|string fts; int32 e; int32 et; int limit;
			 float cutoff|] []
      | Some et, Some s, None ->
	C.fold Q.e_asub_fts_et_super f
	       C.Param.[|string fts; int32 e; int32 et; int32 s;
			 float cutoff|] []
      | Some et, Some s, Some limit ->
	C.fold Q.e_asub_fts_et_super_limit f
	       C.Param.[|string fts; int32 e; int32 et; int32 s; int limit;
			 float cutoff|] []
      end >|= List.rev
    let asub_fts ?entity_type ?super ?(cutoff = 0.0) ?limit =
      asub_fts entity_type super cutoff limit

    let asuper_fts, asuper_fts_cache =
      memo_6lwt @@ fun (et, super, cutoff, limit, e, fts) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup = List.push C.Tuple.(int32 0 tup, float 1 tup) in
      begin match et, super, limit with
      | None, None, None ->
	C.fold Q.e_asuper_fts f
	       C.Param.[|string fts; int32 e;
			 float cutoff|] []
      | None, None, Some limit ->
	C.fold Q.e_asuper_fts_limit f
	       C.Param.[|string fts; int32 e; int limit;
			 float cutoff|] []
      | None, Some s, None ->
	C.fold Q.e_asuper_fts_super f
	       C.Param.[|string fts; int32 e; int32 s;
			 float cutoff|] []
      | None, Some s, Some limit ->
	C.fold Q.e_asuper_fts_super_limit f
	       C.Param.[|string fts; int32 e; int32 s; int limit;
			float cutoff|] []
      | Some et, None, None ->
	C.fold Q.e_asuper_fts f
	       C.Param.[|string fts; int32 e; int32 et;
			 float cutoff|] []
      | Some et, None, Some limit ->
	C.fold Q.e_asuper_fts_limit f
	       C.Param.[|string fts; int32 e; int32 et; int limit;
			 float cutoff|] []
      | Some et, Some s, None ->
	C.fold Q.e_asuper_fts_super f
	       C.Param.[|string fts; int32 e; int32 et; int32 s;
			 float cutoff|] []
      | Some et, Some s, Some limit ->
	C.fold Q.e_asuper_fts_super_limit f
	       C.Param.[|string fts; int32 e; int32 et; int32 s; int limit;
			 float cutoff|] []
      end >|= List.rev
    let asuper_fts ?entity_type ?super ?(cutoff = 0.0) ?limit =
      asuper_fts entity_type super cutoff limit

    let asub_get_bool, asub_get_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, bool 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Bool in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asub_get_bool f C.Param.([|int32 e; int32 at_id|])
	     Map.empty

    let asub_get_int, asub_get_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, int 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asub_get_int f C.Param.([|int32 e; int32 at_id|])
	     Map.empty

    let asub_get_string, asub_get_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, string 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asub_get_string f C.Param.([|int32 e; int32 at_id|]) Map.empty

    let asub_get (type a) e (at : a B.Attribute_type.t)
	  : a Values.t Map.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> asub_get_bool e at.B.Attribute_type.at_id
      | Type.Int -> asub_get_int e at.B.Attribute_type.at_id
      | Type.String -> asub_get_string e at.B.Attribute_type.at_id

    let asuper_get_bool, asuper_get_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, bool 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Bool in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asuper_get_bool f C.Param.([|int32 e; int32 at_id|])
	     Map.empty

    let asuper_get_int, asuper_get_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, int 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asuper_get_int f C.Param.([|int32 e; int32 at_id|])
	     Map.empty

    let asuper_get_string, asuper_get_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db @@ fun (module C : CONNECTION) ->
      let f tup m =
	let e', v = C.Tuple.(int32 0 tup, string 1 tup) in
	let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
	Map.add e' (Values.add v vs) m in
      C.fold Q.e_asuper_get_string f C.Param.([|int32 e; int32 at_id|])
	     Map.empty

    let asuper_get (type a) e (at : a B.Attribute_type.t)
	  : a Values.t Map.t Lwt.t =
      match B.Attribute_type.value_type at with
      | Type.Bool -> asuper_get_bool e at.B.Attribute_type.at_id
      | Type.Int -> asuper_get_int e at.B.Attribute_type.at_id
      | Type.String -> asuper_get_string e at.B.Attribute_type.at_id

    let clear_bool_caches () =
      Cache.clear getattr_bool_cache;
      Cache.clear asub_present_bool_cache;
      Cache.clear asuper_present_bool_cache;
      Cache.clear asub1_bool_cache;
      Cache.clear asuper1_bool_cache;
      Cache.clear asub_get_bool_cache;
      Cache.clear asuper_get_bool_cache

    let clear_int_caches () =
      Cache.clear getattr_int_cache;
      Cache.clear asub_present_int_cache;
      Cache.clear asuper_present_int_cache;
      Cache.clear asub1_int_cache;
      Cache.clear asuper1_int_cache;
      Cache.clear asub2_between_int_cache;
      Cache.clear asuper2_between_int_cache;
      Cache.clear asub_get_int_cache;
      Cache.clear asuper_get_int_cache

    let clear_string_caches () =
      Cache.clear getattr_string_cache;
      Cache.clear asub_present_string_cache;
      Cache.clear asuper_present_string_cache;
      Cache.clear asub1_string_cache;
      Cache.clear asuper1_string_cache;
      Cache.clear asub2_between_string_cache;
      Cache.clear asuper2_between_string_cache;
      Cache.clear asub1_search_cache;
      Cache.clear asuper1_search_cache;
      Cache.clear asub1_search_fts_cache;
      Cache.clear asuper1_search_fts_cache;
      Cache.clear asub_fts_cache;
      Cache.clear asuper_fts_cache;
      Cache.clear asub_get_string_cache;
      Cache.clear asuper_get_string_cache

    let clear_attr_caches (type a) (at : a B.Attribute_type.t) : unit =
      match B.Attribute_type.value_type at with
      | Type.Bool -> clear_bool_caches ()
      | Type.Int -> clear_int_caches ()
      | Type.String -> clear_string_caches ()

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
	C.Param.[|int32 subentity; int32 superentity;
		  int32 subentity; int32 superentity|] >>
      C.exec Q.e_subsume_inclusion
	C.Param.[|int32 subentity; int32 superentity;
		  int32 subentity; int32 superentity|] >|=
      fun () ->
	clear_inclusion_caches ();
	emit_changed subentity `Dsuper;
	emit_changed superentity `Dsub

    let relax_dsub' subentity superentity (module C : CONNECTION) =
      lwt is_subsumed =
	C.find Q.e_is_subsumed C.Tuple.(bool 0)
	       C.Param.[|int32 subentity; int32 superentity|] in
      (if is_subsumed then Lwt.return_unit else
	C.exec Q.e_unsubsume_inclusion
	       C.Param.[|int32 subentity; int32 superentity;
			 int32 subentity; int32 superentity|]) >>
      C.exec Q.e_delete_inclusion
	C.Param.[|int32 subentity; int32 superentity|] >|=
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

    let relax_dsub subentity superentity =
      with_db (relax_dsub' subentity superentity) >>
      lwt subentity_rank = rank subentity in
      lwt superentity_rank = rank superentity in
      if subentity_rank > superentity_rank + 1 then Lwt.return_unit
					       else lower_rank subentity

    let check_uniqueness_for_add e e' new_at new_avs =
      let vt = B.Attribute_type.value_type new_at in
      let new_cond = B.Attribute.In (new_at, Values.of_elements vt new_avs) in
      let is_violated au =
	lwt aff_ats = Attribute_uniqueness.affected au >|=
		      B.Attribute_type.Set.remove (B.Attribute_type.Ex new_at) in
	try_lwt
	  lwt conds = Lwt_list.map_s
	    (fun (B.Attribute_type.Ex at) ->
	      lwt avs = getattr e e' at in
	      if Values.is_empty avs then Lwt.fail Not_found else
	      Lwt.return (B.Attribute.In (at, avs)))
	    (B.Attribute_type.Set.elements aff_ats) in
	  asub_conj e' (new_cond :: conds) >|= not *< Set.is_empty
	with Not_found ->
	  Lwt.return_false in
      lwt violated =
	Attribute_uniqueness.affecting new_at >>=
	B.Attribute_uniqueness.Set.filter_s is_violated in
      if B.Attribute_uniqueness.Set.is_empty violated then Lwt.return_unit else
      Lwt.fail (B.Attribute_uniqueness.Not_unique violated)

    let post_attribute_update (type a) (module C : CONNECTION)
			      e e' (at : a B.Attribute_type.t) =
      clear_attr_caches at;
      emit_changed e `Asuper;
      emit_changed e' `Asub;
      match at.B.Attribute_type.at_value_type with
      | Type.String ->
	C.exec Q.fts_clear C.Param.[|int32 e; int32 e'|] >>
	C.exec Q.fts_insert C.Param.[|int32 e; int32 e'|]
      | _ -> Lwt.return_unit

    let addattr' (module C : CONNECTION)
		 (type a) e e' (at : a B.Attribute_type.t) (xs : a list) =
      let aux q conv =
	Lwt_list.iter_s
	  (fun x ->
	    let p = C.Param.([|int32 e; int32 e';
			       int32 at.B.Attribute_type.at_id; conv x|]) in
	    C.exec q p)
	  xs in
      begin match at.B.Attribute_type.at_value_type with
      | Type.Bool -> aux Q.e_insert_attribution_int
		     (fun x -> C.Param.int (if x then 1 else 0))
      | Type.Int -> aux Q.e_insert_attribution_int C.Param.int
      | Type.String -> aux Q.e_insert_attribution_string C.Param.string
      end >>
      post_attribute_update (module C) e e' at

    let check_mult e e' at =
      lwt et = type_ e in
      lwt et' = type_ e' in
      match_lwt Entity_type.can_asub et et' at with
      | None ->
	lwt etn = Entity_type.name et in
	lwt etn' = Entity_type.name et' in
	lwt_failure_f "addattr: %s is not allowed from %s to %s."
		      at.B.Attribute_type.at_name etn etn'
      | Some mu -> Lwt.return mu

    let addattr (type a) e e' (at : a B.Attribute_type.t) (xs : a list) =
      lwt xs_pres = getattr e e' at in
      lwt xs =
	match_lwt check_mult e e' at with
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
      check_uniqueness_for_add e e' at xs >> (* FIXME: Transaction. *)
      with_db ~transaction:true (fun conn -> addattr' conn e e' at xs)

    let delattr' (module C : CONNECTION)
		 (type a) e e' (at : a B.Attribute_type.t) (xs : a list) =
      let aux q conv =
	Lwt_list.iter_s
	  (fun x ->
	    let p = C.Param.([|int32 e; int32 e';
			       int32 at.B.Attribute_type.at_id; conv x|]) in
	    C.exec q p)
	  xs in
      begin match at.B.Attribute_type.at_value_type with
      | Type.Bool -> aux Q.e_delete_attribution_int
		     (fun x -> C.Param.int (if x then 1 else 0))
      | Type.Int -> aux Q.e_delete_attribution_int C.Param.int
      | Type.String -> aux Q.e_delete_attribution_string C.Param.string
      end >>
      post_attribute_update (module C) e e' at

    let delattr (type a) e e' (at : a B.Attribute_type.t) (xs : a list) =
      lwt xs_pres = getattr e e' at in
      let xs =
	let ht = Hashtbl.create 7 in
	Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
	List.filter
	  (fun x -> if not (Hashtbl.mem ht x) then false else
		    (Hashtbl.remove ht x; true)) xs in
      if xs = [] then Lwt.return_unit else
      with_db ~transaction:true (fun conn -> delattr' conn e e' at xs)

    let setattr (type a) e e' (at : a B.Attribute_type.t) (xs : a list) =
      begin match_lwt check_mult e e' at with
      | Multiplicity.May1 | Multiplicity.Must1 ->
	if List.length xs <= 1 then Lwt.return_unit else
	lwt_failure_f "addattr: Attribute already set.";
      | Multiplicity.May | Multiplicity.Must ->
	Lwt.return_unit
      end >>
      lwt xs_pres = getattr e e' at in
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
      check_uniqueness_for_add e e' at xs_ins >> (* FIXME: Transaction. *)
      with_db ~transaction:true begin fun c ->
	(if xs_del = [] then Lwt.return_unit else delattr' c e e' at xs_del) >>
	(if xs_ins = [] then Lwt.return_unit else addattr' c e e' at xs_ins)
      end

  end

end

let connect uri =
  (module struct
    module M = Make (struct
      let wrap_transaction f (module C : CONNECTION) =
	C.exec Q.begin_ [||] >>
	begin try_lwt
	  lwt r = f (module C : CONNECTION) in
	  C.exec Q.commit [||] >>
	  Lwt.return r
	with exc ->
	  Lwt_log.debug_f "Raised in transaction: %s" (Printexc.to_string exc) >>
	  C.exec Q.rollback [||] >>
	  Lwt.fail exc
	end

      let pool =
	let connect () = Caqti_lwt.connect uri in
	let disconnect (module C : CONNECTION) = C.disconnect () in
	let validate (module C : CONNECTION) = C.validate () in
	let check (module C : CONNECTION) = C.check in
	Caqti_lwt.Pool.create ~validate ~check connect disconnect

      let with_db ~transaction f =
	if transaction then Caqti_lwt.Pool.use (wrap_transaction f) pool
		       else Caqti_lwt.Pool.use f pool
    end)

    module Attribute_type = struct
      include B.Attribute_type
      include M.Attribute_type
    end
    module Attribute_uniqueness = struct
      include B.Attribute_uniqueness
      include M.Attribute_uniqueness
    end
    module Attribute = B.Attribute
    module Entity_type = struct
      include B.Entity_type
      include M.Entity_type
    end
    module Entity = struct
      include B.Entity
      include M.Entity
    end

    module type T = Subsocia_intf.S
      with type 'a Attribute_type.t = 'a Attribute_type.t
       and type Attribute_type.ex = Attribute_type.ex
       and type Attribute_type.Set.t = Attribute_type.Set.t
       and type 'a Attribute_type.Map.t = 'a Attribute_type.Map.t
       and type Attribute.ex = Attribute.ex
       and type Attribute.predicate = Attribute.predicate
       and type Entity_type.t = Entity_type.t
       and type Entity_type.Set.t = Entity_type.Set.t
       and type 'a Entity_type.Map.t = 'a Entity_type.Map.t
       and type Entity.t = Entity.t
       and type Entity.Set.t = Entity.Set.t
       and type 'a Entity.Map.t = 'a Entity.Map.t

    let transaction f =
      M.with_db @@ fun ((module C : CONNECTION) as conn) ->
      let module C' = struct
	module M = Make (struct
	  let lock = Lwt_mutex.create ()
	  let with_db ~transaction f =
	    Lwt_mutex.with_lock lock (fun () -> f conn)
	end)
	module Attribute_type = struct
	  include B.Attribute_type
	  include M.Attribute_type
	end
	module Attribute_uniqueness = struct
	  include B.Attribute_uniqueness
	  include M.Attribute_uniqueness
	end
	module Attribute = B.Attribute
	module Entity_type = struct
	  include B.Entity_type
	  include M.Entity_type
	end
	module Entity = struct
	  include B.Entity
	  include M.Entity
	end
      end in
      f (module C' : T)

    let entity_changed = Int32_event_table.event Entity.changed_event_table
  end : S)
