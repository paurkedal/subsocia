(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf
open Lwt.Infix
open Scanf
open Subsocia_common
open Subsocia_prereq
open Unprime
open Unprime_list
open Unprime_option

let (>|=?) m f =
  m >|= function Ok x -> Ok (f x) | Error _ as r -> r

let (>>=?) m f =
  m >>= function Ok x -> (f x >|= fun y -> Ok y) | Error _ as r -> Lwt.return r

let (>>=??) m f =
  m >>= function Ok x -> f x | Error _ as r -> Lwt.return r

let rec lwt_list_iter_rs f = function
 | [] -> Lwt.return_ok ()
 | x :: xs -> f x >>=?? fun () -> lwt_list_iter_rs f xs

let fetch_grade = 1e-3 *. cache_second
let attribute_type_grade = fetch_grade
let preceq_grade = 1e-2 *. cache_second (* TODO: Highly non-constant. *)

let schema_prefix = ref "subsocia."

let bool_of_int = function 0 -> false | 1 -> true | _ -> assert false

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

module type S = Subsocia_direct_intf.S

module Q = struct

  let env di = function
   | "." ->
      (match Caqti_driver_info.dialect_tag di with
       | `Pgsql -> Caqti_request.L !schema_prefix
       | _ -> Caqti_request.S [])
   | _ -> raise Not_found

  let (-->!) tA (_ : unit Caqti_type.t) = Caqti_request.exec ~env tA
  let (-->) tA tR = Caqti_request.find ~env tA tR
  let (-->?) tA tR = Caqti_request.find_opt ~env tA tR
  let (-->*) tA tR = Caqti_request.collect ~env tA tR

  let table name = Caqti_request.(S [L !schema_prefix; L name])

  open Caqti_type

  (* Attribute Type *)

  let at_by_id = (int32 --> tup3 string string int)
    "SELECT attribute_name, value_type, value_mult FROM $.attribute_type \
     WHERE attribute_type_id = ?"
  let at_by_name = (string -->? tup3 int32 string int)
    "SELECT attribute_type_id, value_type, value_mult FROM $.attribute_type \
     WHERE attribute_name = ?"
  let at_create = (tup4 string string int (option string) --> int32)
    "INSERT INTO $.attribute_type (attribute_name, value_type, value_mult, \
                                   fts_config) \
     VALUES (?, ?, ?, ?) RETURNING attribute_type_id"
  let at_delete = (int32 -->! unit)
    "DELETE FROM $.attribute_type WHERE attribute_type_id = ?"
  let at_all = (unit -->* int32)
    "SELECT attribute_type_id FROM $.attribute_type"

  (* Attribute Uniqueness *)

  let au_all = (unit -->* int32)
    "SELECT attribute_uniqueness_id FROM $.attribute_uniqueness"
  let au_affected = (int32 -->* int32)
    "SELECT attribute_type_id FROM $.attribute_uniqueness \
     WHERE attribute_uniqueness_id = ? ORDER BY attribute_type_id"
  let au_affecting = (int32 -->* int32)
    "SELECT attribute_uniqueness_id FROM $.attribute_uniqueness \
     WHERE attribute_type_id = ?"

  let au_force_query l =
    let open Caqti_request in
    let rec mk_values i =
      let value = S [L"("; P i; L"::int)"] in
      if i = 0 then value :: mk_values (i + 1) else
      if i < l then L", " :: value :: mk_values (i + 1) else [] in
    let values = mk_values 0 in
    S[L"INSERT INTO "; table "attribute_uniqueness";
      L"SELECT attribute_uniqueness_id, attribute_type_id \
        FROM (SELECT nextval('"; table "attribute_uniqueness_id_seq"; L"')) \
                  AS seq(attribute_uniqueness_id) \
              CROSS JOIN (VALUES "; S values; L") AS ats(attribute_type_id) \
        RETURNING attribute_type_id"]

  type _ au_force_cache =
    Au_force_cache : {
      request: ('a, int32, [`One]) Caqti_request.t;
      next: ('a * int32) au_force_cache Lazy.t;
    } -> 'a au_force_cache

  let au_force_cache : unit au_force_cache =
    let rec build : type a. a Caqti_type.t -> a au_force_cache = fun t ->
      let request =
        let l = Caqti_type.length t in
        if l = 0 then Caqti_request.find t int32 "" else
        let q _ = au_force_query l in
        Caqti_request.create t int32 Caqti_mult.one q in
      let next = lazy (build (tup2 t int32)) in
      Au_force_cache {request; next} in
    build Caqti_type.unit

  type au_force_ex =
    Au_force_ex : {
      request: ('a, int32, [`One]) Caqti_request.t;
      param: 'a;
    } -> au_force_ex

  let au_force at_ids =
    let rec loop : type a. a au_force_cache -> a -> int32 list -> au_force_ex =
      fun (Au_force_cache {request; next}) param ->
      (function
       | [] -> Au_force_ex {request; param}
       | id :: ids -> loop (Lazy.force next) (param, id) ids) in
    loop au_force_cache () at_ids

  let au_relax = (int32 -->! unit)
    "DELETE FROM $.attribute_uniqueness WHERE attribute_uniqueness_id = ?"

  (* Entity types *)

  let et_id_of_name = (string -->? int32)
    "SELECT entity_type_id FROM $.entity_type WHERE entity_type_name = ?"
  let et_name_of_id = (int32 --> string)
    "SELECT entity_type_name FROM $.entity_type WHERE entity_type_id = ?"
  let et_create = (string --> int32)
    "INSERT INTO $.entity_type (entity_type_name) VALUES (?) \
     RETURNING entity_type_id"
  let et_delete = (int32 -->! unit)
    "DELETE FROM $.entity_type WHERE entity_type_id = ?"
  let et_all = (unit -->* int32)
    "SELECT entity_type_id FROM $.entity_type"
  let et_entity_name_tmpl = (int32 --> string)
    "SELECT entity_name_tmpl FROM $.entity_type WHERE entity_type_id = ?"
  let et_set_entity_name_tmpl = (tup2 string int32 -->! unit)
    "UPDATE $.entity_type SET entity_name_tmpl = ? WHERE entity_type_id = ?"

  let et_can_dsub = (tup2 int32 int32 -->? tup2 int int)
    "SELECT dsub_mult, dsuper_mult \
     FROM $.inclusion_type \
     WHERE dsub_type_id = ? AND dsuper_type_id = ?"
  let et_dsub = (int32 -->* tup3 int32 int int)
    "SELECT dsub_type_id, dsub_mult, dsuper_mult \
     FROM $.inclusion_type WHERE dsuper_type_id = ?"
  let et_dsuper = (int32 -->* tup3 int32 int int)
    "SELECT dsuper_type_id, dsub_mult, dsuper_mult \
     FROM $.inclusion_type WHERE dsub_type_id = ?"
  let et_dsub_elements = (unit -->* tup4 int32 int32 int int)
    "SELECT dsub_type_id, dsuper_type_id, dsub_mult, dsuper_mult \
     FROM $.inclusion_type"
  let et_allow_dsub = (tup4 int int int32 int32 -->! unit)
    "INSERT INTO $.inclusion_type \
      (dsub_mult, dsuper_mult, dsub_type_id, dsuper_type_id) \
     VALUES (?, ?, ?, ?)"
  let et_disallow_dsub = (tup2 int32 int32 -->! unit)
    "DELETE FROM $.inclusion_type \
     WHERE dsub_type_id = ? AND dsuper_type_id = ?"

  let et_can_attribute = (tup3 int32 int32 int32 --> int)
    "SELECT count(*) FROM $.attribution_type \
     WHERE attribute_type_id = ? AND domain_id = ? AND codomain_id = ?"
  let et_allowed_attributes = (tup2 int32 int32 -->* int32)
    "SELECT attribute_type_id FROM $.attribution_type \
     WHERE domain_id = ? AND codomain_id = ?"
  let et_allowed_preimage = (int32 -->* tup2 int32 int32)
    "SELECT attribute_type_id, domain_id FROM $.attribution_type \
     WHERE codomain_id = ?"
  let et_allowed_image = (int32 -->* tup2 int32 int32)
    "SELECT attribute_type_id, codomain_id FROM $.attribution_type \
     WHERE domain_id = ?"
  let et_allowed_mappings = (int32 -->* tup2 int32 int32)
    "SELECT domain_id, codomain_id FROM $.attribution_type \
     WHERE attribute_type_id = ?"
  let et_allowed_attributions = (unit -->* tup3 int32 int32 int32)
    "SELECT attribute_type_id, domain_id, codomain_id \
     FROM $.attribution_type"
  let et_allow_attribution = (tup3 int32 int32 int32 -->! unit)
    "INSERT INTO $.attribution_type \
      (attribute_type_id, domain_id, codomain_id) \
     VALUES (?, ?, ?)"
  let et_disallow_attribution = (tup3 int32 int32 int32 -->! unit)
    "DELETE FROM $.attribution_type \
     WHERE attribute_type_id = ? AND domain_id = ? AND codomain_id = ?"

  (* Entites *)

  let e_is_dsub = (tup2 int32 int32 --> int)
    "SELECT count(*) FROM $.inclusion WHERE dsub_id = ? AND dsuper_id = ?"

  let e_dsuper_any = (int32 -->* int32)
    "SELECT dsuper_id FROM $.inclusion WHERE dsub_id = ?"
  let e_dsub_any = (int32 -->* int32)
    "SELECT dsub_id FROM $.inclusion WHERE dsuper_id = ?"

  let e_dsuper_typed = (tup2 int32 int32 -->* int32)
    "SELECT entity_id \
     FROM $.inclusion JOIN $.entity ON dsuper_id = entity_id \
     WHERE dsub_id = ? AND entity_type_id = ?"
  let e_dsub_typed = (tup2 int32 int32 -->* int32)
    "SELECT entity_id \
     FROM $.inclusion JOIN $.entity ON dsub_id = entity_id \
     WHERE dsuper_id = ? AND entity_type_id = ?"

  let e_type = (int32 --> int32)
    "SELECT entity_type_id FROM $.entity WHERE entity_id = ?"
  let e_rank = (int32 --> int)
    "SELECT entity_rank FROM $.entity WHERE entity_id = ?"

  let e_set_entity_rank = (tup2 int int32 -->! unit)
    "UPDATE $.entity SET entity_rank = ? WHERE entity_id = ?"

  let e_type_members = (int32 -->* int32)
    "SELECT entity_id FROM $.entity WHERE entity_type_id = ?"

  let e_minimums = (unit -->* int32)
    "SELECT entity_id FROM $.entity \
     WHERE NOT EXISTS \
      (SELECT 0 FROM $.inclusion WHERE dsuper_id = entity_id)"

  let e_select_precedes = (tup3 int int32 int32 --> int)
    "WITH RECURSIVE successors(entity_id) AS ( \
        SELECT i.dsuper_id AS entity_id \
        FROM $.inclusion i \
        JOIN $.entity e ON e.entity_id = i.dsuper_id \
        WHERE i.is_subsumed = false \
          AND e.entity_rank >= $1 \
          AND i.dsub_id = $2 \
      UNION \
        SELECT i.dsuper_id \
        FROM $.inclusion i \
        JOIN $.entity e ON e.entity_id = i.dsuper_id \
        JOIN successors c ON i.dsub_id = c.entity_id \
        WHERE i.is_subsumed = false \
          AND e.entity_rank >= $1 \
     ) \
     SELECT count(*) FROM successors WHERE entity_id = $3 LIMIT 1"

  let e_create_entity = (int32 --> int32)
    "INSERT INTO $.entity (entity_type_id) \
     VALUES (?) RETURNING entity_id"

  let e_delete_entity = (int32 -->! unit)
    "DELETE FROM $.entity WHERE entity_id = ?"

  let e_maybe_insert_inclusion = (tup2 int32 int32 -->! unit)
    "INSERT INTO $.inclusion (dsub_id, dsuper_id) SELECT $1, $2 \
     WHERE NOT EXISTS \
      (SELECT 0 FROM $.inclusion WHERE dsub_id = $1 AND dsuper_id = $2)"

  let e_delete_inclusion = (tup2 int32 int32 -->! unit)
    "DELETE FROM $.inclusion WHERE dsub_id = ? AND dsuper_id = ?"

  (* Params: (sub_id, super_id) *)
  let e_subsume_inclusion = (tup2 int32 int32 -->! unit)
    "WITH RECURSIVE \
      lb(id) AS ( \
          SELECT $1::integer AS id \
        UNION \
          SELECT i.dsub_id AS id \
          FROM lb JOIN $.inclusion i ON i.dsuper_id = lb.id \
          WHERE i.is_subsumed = false \
      ), \
      ub(id) AS ( \
          SELECT $2::integer AS id \
        UNION \
          SELECT i.dsuper_id AS id \
          FROM ub JOIN $.inclusion i ON i.dsub_id = ub.id \
          WHERE i.is_subsumed = false \
      ) \
    UPDATE $.inclusion i SET is_subsumed = true \
    WHERE (i.dsub_id <> $1::integer OR i.dsuper_id <> $2::integer) \
      AND i.dsub_id IN (SELECT * FROM lb) \
      AND i.dsuper_id IN (SELECT * FROM ub) \
      AND i.is_subsumed = false"

  let e_is_subsumed = (tup2 int32 int32 --> bool)
    "SELECT is_subsumed FROM $.inclusion WHERE dsub_id = ? AND dsuper_id = ?"

  (* Params: (sub_id, super_id) *)
  let e_unsubsume_inclusion = (tup2 int32 int32 -->! unit)
    "WITH RECURSIVE \
      lb(id) AS ( \
          SELECT $1::integer AS id \
        UNION \
          SELECT i.dsub_id AS id \
          FROM lb JOIN $.inclusion i ON i.dsuper_id = lb.id \
          WHERE is_subsumed = false \
      ), \
      ub(id) AS ( \
          SELECT $2::integer AS id \
        UNION \
          SELECT i.dsuper_id AS id \
          FROM ub JOIN $.inclusion i ON i.dsub_id = ub.id \
          WHERE is_subsumed = false \
      ) \
    UPDATE $.inclusion i SET is_subsumed = false \
      WHERE i.is_subsumed = true \
        AND i.dsub_id IN (SELECT * FROM lb) \
        AND i.dsuper_id IN (SELECT * FROM ub) \
        AND NOT EXISTS ( \
          WITH RECURSIVE acc(id) AS ( \
              SELECT i.dsub_id AS id \
            UNION \
              SELECT j.dsuper_id AS id \
              FROM $.inclusion j JOIN acc ON j.dsub_id = acc.id \
              WHERE (j.dsub_id <> $1::integer OR j.dsuper_id <> $2::integer) \
                AND (j.dsub_id <> i.dsub_id OR j.dsuper_id <> i.dsuper_id) \
          ) \
          SELECT 1 FROM acc WHERE acc.id = i.dsuper_id \
        )"

  let e_select_attribution_bool = (tup3 int32 int32 int32 -->* bool)
    "SELECT value FROM $.attribution_bool \
     WHERE input_id = ? AND output_id = ? AND attribute_type_id = ?"
  let e_select_attribution_int = (tup3 int32 int32 int32 -->* int)
    "SELECT value FROM $.attribution_int \
     WHERE input_id = ? AND output_id = ? AND attribute_type_id = ?"
  let e_select_attribution_string = (tup3 int32 int32 int32 -->* string)
    "SELECT value FROM $.attribution_string \
     WHERE input_id = ? AND output_id = ? AND attribute_type_id = ?"

  let e_insert_attribution_bool = (tup4 int32 bool int32 int32 -->! unit)
    "INSERT INTO $.attribution_bool \
      (attribute_type_id, value, input_id, output_id) \
     VALUES (?, ?, ?, ?)"
  let e_insert_attribution_int = (tup4 int32 int int32 int32 -->! unit)
    "INSERT INTO $.attribution_int \
      (attribute_type_id, value, input_id, output_id) \
     VALUES (?, ?, ?, ?)"
  let e_insert_attribution_string = (tup4 int32 string int32 int32 -->! unit)
    "INSERT INTO $.attribution_string \
      (attribute_type_id, value, input_id, output_id) \
     VALUES (?, ?, ?, ?)"

  let e_delete_attribution_bool = (tup4 int32 bool int32 int32 -->! unit)
    "DELETE FROM $.attribution_bool \
     WHERE attribute_type_id = ? AND value = ? \
       AND input_id = ? AND output_id = ?"
  let e_delete_attribution_int = (tup4 int32 int int32 int32 -->! unit)
    "DELETE FROM $.attribution_int \
     WHERE attribute_type_id = ? AND value = ? \
       AND input_id = ? AND output_id = ?"
  let e_delete_attribution_string = (tup4 int32 string int32 int32 -->! unit)
    "DELETE FROM $.attribution_string \
     WHERE attribute_type_id = ? AND value = ? \
       AND input_id = ? AND output_id = ?"

  let ap1_ops = [|"="; "<="; ">="|]
  let ap1_eq = 0
  let ap1_leq = 1
  let ap1_geq = 2

  let e_asub_present_bool = (tup2 int32 int32 -->* int32)
    "SELECT output_id FROM $.attribution_bool \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_asub_present_int = (tup2 int32 int32 -->* int32)
    "SELECT output_id FROM $.attribution_int \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_asub_present_string = (tup2 int32 int32 -->* int32)
    "SELECT output_id FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ?"

  let e_asuper_present_bool = (tup2 int32 int32 -->* int32)
    "SELECT input_id FROM $.attribution_bool \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_asuper_present_int = (tup2 int32 int32 -->* int32)
    "SELECT input_id FROM $.attribution_int \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_asuper_present_string = (tup2 int32 int32 -->* int32)
    "SELECT input_id FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ?"

  let e_asub1_bool = ap1_ops |> Array.map @@ fun op ->
    (tup3 int32 int32 bool -->* int32)
    ("SELECT output_id FROM $.attribution_bool \
      WHERE input_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asub1_int = ap1_ops |> Array.map @@ fun op ->
    (tup3 int32 int32 int -->* int32)
    ("SELECT output_id FROM $.attribution_int \
      WHERE input_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asub1_string = ap1_ops |> Array.map @@ fun op ->
    (tup3 int32 int32 string -->* int32)
    ("SELECT output_id FROM $.attribution_string \
      WHERE input_id = ? AND attribute_type_id = ? AND value "^op^" ?")

  let e_asuper1_bool = ap1_ops |> Array.map @@ fun op ->
    (tup3 int32 int32 bool -->* int32)
    ("SELECT input_id FROM $.attribution_bool \
      WHERE output_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asuper1_int = ap1_ops |> Array.map @@ fun op ->
    (tup3 int32 int32 int -->* int32)
    ("SELECT input_id FROM $.attribution_int \
      WHERE output_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asuper1_string = ap1_ops |> Array.map @@ fun op ->
    (tup3 int32 int32 string -->* int32)
    ("SELECT input_id FROM $.attribution_string \
      WHERE output_id = ? AND attribute_type_id = ? AND value "^op^" ?")

  let e_asub2_between_int = (tup4 int32 int32 int int -->* int32)
    "SELECT output_id FROM $.attribution_int \
     WHERE input_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"
  let e_asub2_between_string = (tup4 int32 int32 string string -->* int32)
    "SELECT output_id FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"

  let e_asuper2_between_int = (tup4 int32 int32 int int -->* int32)
    "SELECT input_id FROM $.attribution_int \
     WHERE output_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"
  let e_asuper2_between_string = (tup4 int32 int32 string string -->* int32)
    "SELECT input_id FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"

  let e_asub1_search = (tup3 int32 int32 string -->* int32)
    "SELECT output_id FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ? \
       AND value SIMILAR TO ?"
  let e_asuper1_search = (tup3 int32 int32 string -->* int32)
    "SELECT input_id FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ? \
       AND value SIMILAR TO ?"

  let e_asub1_search_fts = (tup2 int32 string -->* int32)
    "SELECT output_id FROM $.attribution_string_fts \
     WHERE input_id = ? \
       AND fts_vector @@ to_tsquery(fts_config::regconfig, ?)"
  let e_asuper1_search_fts = (tup2 int32 string -->* int32)
    "SELECT input_id FROM $.attribution_string_fts \
     WHERE output_id = ? \
       AND fts_vector @@ to_tsquery(fts_config::regconfig, ?)"

  let _asub_fts with_et with_super with_lim tA =
    (tup4 string int32 tA float -->* tup2 int32 float) @@
    sprintf
      "SELECT * FROM \
        (SELECT a.output_id, \
                ts_rank(fts_vector, to_tsquery(fts_config::regconfig, ?)) AS r \
         FROM $.attribution_string_fts AS a%s%s WHERE a.input_id = ?%s%s \
         ORDER BY r DESC%s) AS sq \
       WHERE r > ?"
      (if with_et then " JOIN $.entity ON a.output_id = entity_id" else "")
      (if with_super then " JOIN $.transitive_reflexive_inclusion AS c \
                              ON c.tsub_id = a.output_id" else "")
      (if with_et then " AND entity_type_id = ?" else "")
      (if with_super then " AND c.tsuper_id = ?" else "")
      (if with_lim then " LIMIT ?" else "")

  let _asuper_fts with_et with_super with_lim tA =
    (tup4 string int32 tA float -->* tup2 int32 float) @@
    sprintf
      "SELECT * FROM \
        (SELECT a.input_id, \
                ts_rank(fts_vector, to_tsquery(fts_config::regconfig, ?)) AS r \
         FROM $.attribution_string_fts AS a%s%s WHERE a.output_id = ?%s%s \
         ORDER BY r DESC%s) AS sq \
       WHERE r > ?"
      (if with_et then " JOIN $.entity ON a.input_id = entity_id" else "")
      (if with_super then " JOIN $.transitive_reflexive_inclusion AS c \
                              ON c.tsub_id = a.input_id" else "")
      (if with_et then " AND entity_type_id = ?" else "")
      (if with_super then " AND c.tsuper_id = ?" else "")
      (if with_lim then " LIMIT ?" else "")

  let e_asub_fts                = _asub_fts   false false false unit
  let e_asuper_fts              = _asuper_fts false false false unit
  let e_asub_fts_lim            = _asub_fts   false false true  int
  let e_asuper_fts_lim          = _asuper_fts false false true  int
  let e_asub_fts_super          = _asub_fts   false true  false int32
  let e_asuper_fts_super        = _asuper_fts false true  false int32
  let e_asub_fts_super_lim      = _asub_fts   false true  true  (tup2 int32 int)
  let e_asuper_fts_super_lim    = _asuper_fts false true  true  (tup2 int32 int)
  let e_asub_fts_et             = _asub_fts   true  false false int32
  let e_asuper_fts_et           = _asuper_fts true  false false int32
  let e_asub_fts_et_lim         = _asub_fts   true  false true  (tup2 int32 int)
  let e_asuper_fts_et_lim       = _asuper_fts true  false true  (tup2 int32 int)
  let e_asub_fts_et_super       = _asub_fts   true  true  false (tup2 int32 int32)
  let e_asuper_fts_et_super     = _asuper_fts true  true  false (tup2 int32 int32)
  let e_asub_fts_et_super_lim   = _asub_fts   true  true  true  (tup3 int32 int32 int)
  let e_asuper_fts_et_super_lim = _asuper_fts true  true  true  (tup3 int32 int32 int)

  let e_mapping1_bool = (tup2 int32 int32 -->* tup2 int32 bool)
    "SELECT output_id, value FROM $.attribution_bool \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_mapping1_int = (tup2 int32 int32 -->* tup2 int32 int)
    "SELECT output_id, value FROM $.attribution_int \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_mapping1_string = (tup2 int32 int32 -->* tup2 int32 string)
    "SELECT output_id, value FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ?"

  let e_premapping1_bool = (tup2 int32 int32 -->* tup2 int32 bool)
    "SELECT input_id, value FROM $.attribution_bool \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_premapping1_int = (tup2 int32 int32 -->* tup2 int32 int)
    "SELECT input_id, value FROM $.attribution_int \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_premapping1_string = (tup2 int32 int32 -->* tup2 int32 string)
    "SELECT input_id, value FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ?"

  let fts_clear = (tup2 int32 int32 -->! unit)
    "DELETE FROM subsocia.attribution_string_fts \
     WHERE input_id = ? AND output_id = ?"
  let fts_insert = (tup2 int32 int32 -->! unit)
    "INSERT INTO subsocia.attribution_string_fts \
                  (input_id, output_id, fts_config, fts_vector) \
     SELECT a.input_id, a.output_id, at.fts_config, \
            to_tsvector(at.fts_config::regconfig, string_agg(value, '$')) \
     FROM subsocia.attribution_string AS a \
       NATURAL JOIN subsocia.attribute_type AS at \
     WHERE NOT at.fts_config IS NULL \
       AND a.input_id = ? AND a.output_id = ? \
     GROUP BY a.input_id, a.output_id, at.fts_config"
end

module type CACHE = sig
  type ('a, 'b) t
  val create :  cache_metric: Prime_cache_metric.t -> int -> ('a, 'b) t
  val clear : ('a, 'b) t -> unit
  val find : ('a, 'b) t -> 'a -> 'b
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
        let%lwt y = f x in
        Prime_cache.replace cache fetch_grade x y;
        Lwt.return y in
    g, cache

  let memo_lwt_conn f =
    let cache = Prime_cache.create ~cache_metric 23 in
    let g ?conn x =
      try Lwt.return (Prime_cache.find cache x)
      with Not_found ->
        let%lwt y = f ?conn x in
        Prime_cache.replace cache fetch_grade x y;
        Lwt.return y in
    g, cache
end

module Disabled_cache = struct
  type ('a, 'b) t = unit
  let create ~cache_metric:_ _n = ()
  let clear _ = ()
  let find _ _ = raise Not_found
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
(*
let memo_5lwt f = let g, c = memo_1lwt f in
                  (fun x0 x1 x2 x3 x4 -> g (x0, x1, x2, x3, x4)), c
*)
let memo_6lwt f = let g, c = memo_1lwt f in
                  (fun x0 x1 x2 x3 x4 x5 -> g (x0, x1, x2, x3, x4, x5)), c

module B = struct
  module Attribute_type = struct
    type soid = int32
    type 'a t = {
      at_id : int32;
      at_name : string;
      at_value_type : 'a Type.t;
      at_value_mult : Multiplicity.t;
      at_beacon : Beacon.t;
    }
    type ex = Ex : 'a t -> ex

    let value_type at = at.at_value_type
    let value_mult at = at.at_value_mult

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

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.at_soid_format id
      let of_string s = sscanf s Subsocia_internal.at_soid_format (fun id -> id)
      let compare = compare
    end
  end

  module Attribute_uniqueness = struct
    type soid = int32
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.au_soid_format id
      let of_string s = sscanf s Subsocia_internal.au_soid_format (fun id -> id)
      let compare = compare
    end

    exception Not_unique of Set.t
  end

  module Relation = struct
    type t =
      | Inter : t list -> t
      | Present : 'a Attribute_type.t -> t
      | Eq : 'a Attribute_type.t * 'a -> t
      | In : 'a Attribute_type.t * 'a Values.t -> t
      | Leq : 'a Attribute_type.t * 'a -> t
      | Geq : 'a Attribute_type.t * 'a -> t
      | Between : 'a Attribute_type.t * 'a * 'a -> t
      | Search : string Attribute_type.t * string -> t
      | Search_fts : string -> t
  end

  module Entity_type = struct
    type soid = int32
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.et_soid_format id
      let of_string s = sscanf s Subsocia_internal.et_soid_format (fun id -> id)
      let compare = compare
    end

    let compare = Int32.compare
  end

  module Entity = struct
    type soid = int32
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.e_soid_format id
      let of_string s = sscanf s Subsocia_internal.e_soid_format (fun id -> id)
      let compare = compare
    end
  end
end

module type CONNECTION = Caqti_lwt.CONNECTION

module type Param = sig
  val with_db :
    transaction: bool ->
    ((module CONNECTION) -> ('a, ([> Caqti_error.t] as 'e)) result Lwt.t) ->
    ('a, 'e) result Lwt.t
end

module Make (P : Param) = struct
  let inclusion_cache = Cache.create ~cache_metric 61

  let with_db ?conn ?(transaction = false) f =
    (match conn with
     | None -> P.with_db ~transaction f
     | Some conn -> f conn)

  let with_db_exn ?conn ?transaction f =
    with_db ?conn ?transaction f >>=
    (function
     | Ok y -> Lwt.return y
     | Error err ->
        Lwt_log.debug (Caqti_error.show err) >>= fun () ->
        Lwt.fail (Caqti_error.Exn err))

  module Attribute_type = struct
    open B.Attribute_type

    let soid at = Lwt.return at.at_id
    let name at = Lwt.return at.at_name

    let of_soid', of_soid_cache = Cache.memo_lwt_conn @@ fun ?conn at_id ->
      with_db_exn ?conn @@ fun (module C : CONNECTION) ->
      C.find Q.at_by_id at_id >|=? fun (at_name, value_type, value_mult) ->
      let Type.Ex at_value_type = Type.of_string value_type in
      let at_value_mult = Multiplicity.of_int value_mult in
      Beacon.embed attribute_type_grade @@ fun at_beacon ->
      Ex {at_id; at_name; at_value_type; at_value_mult; at_beacon}

    let of_soid id = of_soid' id

    let of_name, of_name_cache = memo_1lwt @@ fun at_name ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.find_opt Q.at_by_name at_name >|=?
      Option.map begin fun (at_id, value_type, value_mult) ->
        let Type.Ex at_value_type = Type.of_string value_type in
        let at_value_mult = Multiplicity.of_int value_mult in
        Beacon.embed attribute_type_grade @@ fun at_beacon ->
        Ex {at_id; at_name; at_value_type; at_value_mult; at_beacon}
      end

    let create
        : type a. ?mult: Multiplicity.t -> a Type.t -> string -> a t Lwt.t =
      fun ?(mult = Multiplicity.May) vt at_name ->
      let fts =
        match vt with
        | Type.String ->
          let len = String.length at_name in
          if len < 3 || at_name.[len - 3] <> '.' then Some "simple" else
          Some (tsconfig_of_lang2 (String.sub at_name (len - 2) 2))
        | _ -> None in
      with_db_exn @@ fun ((module C : CONNECTION) as conn) ->
      C.find Q.at_create
        (at_name, Type.to_string vt, Multiplicity.to_int mult, fts)
        >>=? of_soid' ~conn >|=? assert_coerce vt

    let delete at =
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.exec Q.at_delete at.at_id

    let all () =
      with_db_exn @@ fun ((module C : CONNECTION) as conn) ->
      C.fold Q.at_all List.cons () [] >>=? fun at_ids ->
      Pwt_list.fold_s
        (fun id acc -> of_soid' ~conn id >|= fun at -> Set.add at acc)
        at_ids Set.empty

    (**/**)
    let id at = at.at_id
    let of_id = of_soid
  end

  module Attribute_uniqueness = struct
    open B.Attribute_uniqueness

    let of_soid = Lwt.return
    let soid = Lwt.return

    let all, all_cache = memo_1lwt @@ fun () ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.au_all Set.add () Set.empty

    let affecting', affecting_cache = memo_1lwt @@ fun at_id ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.au_affecting Set.add at_id Set.empty

    let affecting at = affecting' (Attribute_type.id at)

    let affected, affected_cache = memo_1lwt @@ fun au_id ->
      begin
        with_db_exn @@ fun (module C : CONNECTION) ->
        C.fold Q.au_affected List.cons au_id []
      end
      >>= Lwt_list.rev_map_s Attribute_type.of_id
      >|= B.Attribute_type.Set.of_ordered_elements

    let find atset =
      let B.Attribute_type.Ex at = B.Attribute_type.Set.min_elt_exn atset in
      affecting at >>=
      Set.filter_s (fun au -> affected au >|= B.Attribute_type.Set.equal atset)
        >|= fun auset ->
      (match Set.cardinal auset with
       | 0 -> None
       | 1 -> Some (Set.min_elt_exn auset)
       | _ -> assert false)

    let force atset =
      (* TODO: Enforce non-duplication of constraints. *)
      let ats = B.Attribute_type.Set.elements atset in
      let Q.(Au_force_ex {request; param}) =
        Q.au_force
          (List.map (fun (B.Attribute_type.Ex at) -> Attribute_type.id at) ats)
      in
      begin
        with_db_exn @@ fun (module C : CONNECTION) ->
        C.fold request (fun at_id _ -> Some at_id) param None
      end >|= fun au_id_opt ->
      assert (au_id_opt <> None);
      Cache.clear all_cache;
      Cache.clear affecting_cache;
      Option.get au_id_opt

    let relax au_id =
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.exec Q.au_relax au_id >|=? fun () ->
      Cache.clear all_cache;
      Cache.remove affected_cache au_id;
      Cache.clear affecting_cache

    (**/**)
    let id au = au
    let of_id = of_soid
  end

  module Attribution_sql = Subsocia_attribution_sql.Make (struct
    module Attribute_type = struct
      include B.Attribute_type
      include Attribute_type
    end
    module Relation = B.Relation
  end)

  module Entity_type = struct
    open B.Entity_type

    let of_soid = Lwt.return
    let soid = Lwt.return

    let of_name, of_name_cache =
      memo_1lwt @@ fun name ->
      let name = if name = "unit" then "root" else name in (* TODO: Remove *)
      with_db_exn @@ fun (module C) ->
      C.find_opt Q.et_id_of_name name

    let name, name_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun (module C) ->
      C.find Q.et_name_of_id et

    let create etn =
      with_db_exn @@ fun (module C) ->
      C.find Q.et_create etn

    let delete et =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_delete et

    let all () =
      with_db_exn @@ fun (module C) ->
      C.fold Q.et_all Set.add () Set.empty

    let entity_name_tmpl, entity_name_tmpl_cache = memo_1lwt @@ fun et ->
      with_db_exn @@ fun (module C) ->
      C.find Q.et_entity_name_tmpl et

    let set_entity_name_tmpl et name =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_set_entity_name_tmpl (name, et)

    let can_dsub, can_dsub_cache =
      memo_2lwt @@ fun (et0, et1) ->
      (with_db_exn @@ fun (module C) -> C.find_opt Q.et_can_dsub (et0, et1))
      >|= Option.map (fun (m0, m1) -> Multiplicity.(of_int m0, of_int m1))

    let dsub, dsub_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun (module C) ->
      let aux (id, m0, m1) = Map.add id Multiplicity.(of_int m0, of_int m1) in
      C.fold Q.et_dsub aux et Map.empty

    let dsuper, dsuper_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun (module C) ->
      let aux (id, m0, m1) = Map.add id Multiplicity.(of_int m0, of_int m1) in
      C.fold Q.et_dsuper aux et Map.empty

    let dsub_elements () =
      with_db_exn @@ fun (module C) ->
      let aux (id0, id1, m0, m1) acc =
        (id0, id1, Multiplicity.of_int m0, Multiplicity.of_int m1) :: acc in
      C.fold Q.et_dsub_elements aux () []

    let allow_dsub mu0 mu1 et0 et1 =
      with_db_exn @@ fun (module C) ->
      let mu0, mu1 = Multiplicity.(to_int mu0, to_int mu1) in
      C.exec Q.et_allow_dsub (mu0, mu1, et0, et1)

    let disallow_dsub et0 et1 =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_disallow_dsub (et0, et1)

    let allowed_attributes, allowed_attributes_cache =
      memo_2lwt @@ fun (et, et') ->
      with_db_exn @@ fun ((module C) as conn) ->
      let aux at_map at_id =
        let%lwt at = Attribute_type.of_soid' ~conn at_id in
        Lwt.return (B.Attribute_type.Set.add at at_map) in
      C.fold Q.et_allowed_attributes List.cons (et, et') [] >>=? fun at_ids ->
      Lwt_list.fold_left_s aux B.Attribute_type.Set.empty at_ids

    let allowed_preimage, allowed_preimage_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun ((module C) as conn) ->
      let aux acc (at_id, et) =
        let%lwt at = Attribute_type.of_soid' ~conn at_id in
        let ats' = try Map.find et acc with Not_found -> [] in
        Lwt.return (Map.add et (at :: ats') acc) in
      C.fold Q.et_allowed_preimage List.cons et [] >>=? fun bindings ->
      Lwt_list.fold_left_s aux Map.empty bindings

    let allowed_image, allowed_image_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun ((module C) as conn) ->
      let aux acc (at_id, et) =
        let%lwt at = Attribute_type.of_soid' ~conn at_id in
        let ats' = try Map.find et acc with Not_found -> [] in
        Lwt.return (Map.add et (at :: ats') acc) in
      C.fold Q.et_allowed_image List.cons et [] >>=? fun bindings ->
      Lwt_list.fold_left_s aux Map.empty bindings

    let allowed_mappings', allowed_mappings_cache =
      memo_1lwt @@ fun (B.Attribute_type.Ex at) ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.et_allowed_mappings List.cons at.B.Attribute_type.at_id []

    let allowed_mappings at = allowed_mappings' (B.Attribute_type.Ex at)

    let can_attribute', can_attribute_cache =
      memo_3lwt @@ fun (at, et, et') ->
      with_db_exn @@ fun (module C) ->
      C.find Q.et_can_attribute (at, et, et') >|=? bool_of_int

    let can_attribute at et et' =
      can_attribute' at.B.Attribute_type.at_id et et'

    let allowed_attributions () =
      with_db_exn @@ fun ((module C) as conn) ->
      let aux (at_id, et0, et1) =
        Attribute_type.of_soid' ~conn at_id >|= fun at -> (at, et0, et1) in
      C.fold Q.et_allowed_attributions List.cons () [] >>=? Lwt_list.map_s aux

    let allow_attribution at et et' =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_allow_attribution (at.B.Attribute_type.at_id, et, et')

    let disallow_attribution at et et' =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_disallow_attribution (at.B.Attribute_type.at_id, et, et')

    let display_name ~langs:_ ?pl:_ = name (* FIXME *)

    (**/**)
    let of_id et = Lwt.return et
    let id et = et
  end

  module Entity = struct
    open B.Entity

    let changed_event_table = Int32_event_table.create 97
    let emit_changed = Int32_event_table.emit changed_event_table

    let compare = Int32.compare

    let of_soid = Lwt.return
    let soid = Lwt.return

    let root_id = 1l
    let is_root e = Lwt.return (e = root_id)
    let root = of_soid root_id

    let entity_type, entity_type_cache = memo_1lwt @@ fun e ->
      with_db_exn @@ fun (module C) ->
      C.find Q.e_type e

    let rank, rank_cache = memo_1lwt @@ fun e ->
      with_db_exn @@ fun (module C) ->
      C.find Q.e_rank e

    let type_members, type_members_cache = memo_1lwt @@ fun entity_type_id ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_type_members Set.add entity_type_id Set.empty

    let minimums, minimums_cache = memo_0lwt @@ fun () ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_minimums Set.add () Set.empty

    let dsub_any, dsub_any_cache = memo_1lwt @@ fun e ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsub_any Set.add e Set.empty

    let dsuper_any, dsuper_any_cache = memo_1lwt @@ fun e ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsuper_any Set.add e Set.empty

    let dsub_typed, dsub_typed_cache = memo_2lwt @@ fun (et, e) ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsub_typed Set.add (e, et) Set.empty

    let dsuper_typed, dsuper_typed_cache = memo_2lwt @@ fun (et, e) ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsuper_typed Set.add (e, et) Set.empty

    let dsub ?et e =
      match et with None -> dsub_any e | Some et -> dsub_typed et e

    let dsuper ?et e =
      match et with None -> dsuper_any e | Some et -> dsuper_typed et e

    let create entity_type =
      with_db_exn @@ fun (module C) ->
      C.find Q.e_create_entity entity_type

    let delete e =
      with_db_exn @@ fun (module C) ->
      C.exec Q.e_delete_entity e >|=? fun r ->
      Cache.clear minimums_cache;
      Cache.clear dsub_any_cache;
      Cache.clear dsub_typed_cache;
      r

    let is_dsub, is_dsub_cache = memo_2lwt @@ fun (e, e') ->
      with_db_exn @@ fun (module C) ->
      C.find Q.e_is_dsub (e, e') >|=? bool_of_int

    let is_sub subentity superentity =
      if subentity = superentity then Lwt.return_true else
      let k = subentity, superentity in
      try Lwt.return (Cache.find inclusion_cache k)
      with Not_found ->
        let%lwt r_lim = rank superentity in
        let%lwt c = with_db_exn @@ fun (module C) ->
          C.find Q.e_select_precedes (r_lim, subentity, superentity)
            >|=? bool_of_int in
        Cache.replace inclusion_cache preceq_grade k c;
        Lwt.return c

    let clear_inclusion_caches () =
      Cache.clear minimums_cache;
      Cache.clear is_dsub_cache;
      Cache.clear dsub_any_cache;
      Cache.clear dsub_typed_cache;
      Cache.clear dsuper_any_cache;
      Cache.clear dsuper_typed_cache;
      Cache.clear inclusion_cache

    let get_values_bool, get_values_bool_cache =
      memo_3lwt @@ fun (e, e', at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_attribution_bool Values.add (e, e', at_id)
             (Values.empty Type.Bool)

    let get_values_int, get_values_int_cache =
      memo_3lwt @@ fun (e, e', at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_attribution_int Values.add (e, e', at_id)
             (Values.empty Type.Int)

    let get_values_string, get_values_string_cache =
      memo_3lwt @@ fun (e, e', at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_select_attribution_string Values.add (e, e', at_id)
             (Values.empty Type.String)

    let get_values (type a) (at : a B.Attribute_type.t) e e'
        : a Values.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> get_values_bool e e' at.B.Attribute_type.at_id
       | Type.Int -> get_values_int e e' at.B.Attribute_type.at_id
       | Type.String -> get_values_string e e' at.B.Attribute_type.at_id)

    (* TODO: Cache? *)
    let image_generic p es =
      (match%lwt Attribution_sql.select_image p es with
       | Attribution_sql.Empty -> Lwt.return Set.empty
       | Attribution_sql.Request (request, param) ->
          with_db_exn @@ fun (module C : CONNECTION) ->
          C.fold request Set.add param Set.empty)

    (* TODO: Cache? *)
    let preimage_generic p es =
      (match%lwt Attribution_sql.select_preimage p es with
       | Attribution_sql.Empty -> Lwt.return Set.empty
       | Attribution_sql.Request (request, param) ->
          with_db_exn @@ fun (module C : CONNECTION) ->
          C.fold request Set.add param Set.empty)

    let asub_conj e ps = image_generic (B.Relation.Inter ps) [e]
    (* let asuper_conj e ps = preimage_generic (B.Relation.Inter ps) [e] *)

    let asub_present_bool, asub_present_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub_present_bool Set.add (e, at_id) Set.empty

    let asub_present_int, asub_present_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub_present_int Set.add (e, at_id) Set.empty

    let asub_present_string, asub_present_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub_present_string Set.add (e, at_id) Set.empty

    let asub1_bool, asub1_bool_cache =
      memo_4lwt @@ fun (op, at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub1_bool.(op) Set.add (e, at_id, x) Set.empty

    let asub1_int, asub1_int_cache =
      memo_4lwt @@ fun (op, at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub1_int.(op) Set.add (e, at_id, x) Set.empty

    let asub1_string, asub1_string_cache =
      memo_4lwt @@ fun (op, at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub1_string.(op) Set.add (e, at_id, x) Set.empty

    let asub1 op (type a) (at : a B.Attribute_type.t) (x : a) e : Set.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> asub1_bool op (Attribute_type.id at) x e
       | Type.Int -> asub1_int op (Attribute_type.id at) x e
       | Type.String -> asub1_string op (Attribute_type.id at) x e)

    let asub2_between_int, asub2_between_int_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub2_between_int Set.add (e, at_id, x0, x1) Set.empty

    let asub2_between_string, asub2_between_string_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub2_between_string Set.add (e, at_id, x0, x1) Set.empty

    let asub2_between (type a) e (at : a B.Attribute_type.t)
        : a -> a -> Set.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> fun x0 x1 ->
          if x0 = x1 then
            asub1_bool Q.ap1_eq at.B.Attribute_type.at_id x0 e else
          if x0 < x1 then
            asub_present_bool at.B.Attribute_type.at_id e else
          Lwt.return Set.empty
       | Type.Int -> asub2_between_int e at.B.Attribute_type.at_id
       | Type.String -> asub2_between_string e at.B.Attribute_type.at_id)

    let asub1_search, asub1_search_cache =
      memo_3lwt @@ fun (at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub1_search Set.add (e, at_id, x) Set.empty

    let asub1_search_fts, asub1_search_fts_cache =
      memo_2lwt @@ fun (x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub1_search_fts Set.add (e, x) Set.empty

    let image1 p e =
      (match p with
       | B.Relation.Inter _ | B.Relation.In _ -> image_generic p [e]
       | B.Relation.Present at ->
          (match B.Attribute_type.value_type at with
           | Type.Bool -> asub_present_bool e at.B.Attribute_type.at_id
           | Type.Int -> asub_present_int e at.B.Attribute_type.at_id
           | Type.String -> asub_present_string e at.B.Attribute_type.at_id)
       | B.Relation.Eq (at, x) -> asub1 Q.ap1_eq at x e
       | B.Relation.Leq (at, x) -> asub1 Q.ap1_leq at x e
       | B.Relation.Geq (at, x) -> asub1 Q.ap1_geq at x e
       | B.Relation.Between (at, x0, x1) -> asub2_between e at x0 x1
       | B.Relation.Search (at, x) ->
          asub1_search (Attribute_type.id at) x e
       | B.Relation.Search_fts x -> asub1_search_fts x e)

    let image1_eq at e = asub1 Q.ap1_eq at e

    let asuper_present_bool, asuper_present_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper_present_bool Set.add (e, at_id) Set.empty

    let asuper_present_int, asuper_present_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper_present_int Set.add (e, at_id) Set.empty

    let asuper_present_string, asuper_present_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper_present_string Set.add (e, at_id) Set.empty

    let asuper1_bool, asuper1_bool_cache =
      memo_4lwt @@ fun (op, at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper1_bool.(op) Set.add (e, at_id, x) Set.empty

    let asuper1_int, asuper1_int_cache =
      memo_4lwt @@ fun (op, at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper1_int.(op) Set.add (e, at_id, x) Set.empty

    let asuper1_string, asuper1_string_cache =
      memo_4lwt @@ fun (op, at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper1_string.(op) Set.add (e, at_id, x) Set.empty

    let asuper1 op (type a) (at : a B.Attribute_type.t) (x : a) e
        : Set.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> asuper1_bool op (Attribute_type.id at) x e
       | Type.Int -> asuper1_int op (Attribute_type.id at) x e
       | Type.String -> asuper1_string op (Attribute_type.id at) x e)

    let asuper2_between_int, asuper2_between_int_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper2_between_int Set.add (e, at_id, x0, x1) Set.empty

    let asuper2_between_string, asuper2_between_string_cache =
      memo_4lwt @@ fun (e, at_id, x0, x1) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper2_between_string Set.add (e, at_id, x0, x1) Set.empty

    let asuper2_between (type a) e (at : a B.Attribute_type.t)
        : a -> a -> Set.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> fun x0 x1 ->
          if x0 = x1 then
            asuper1_bool Q.ap1_eq at.B.Attribute_type.at_id x0 e else
          if x0 < x1 then
            asuper_present_bool at.B.Attribute_type.at_id e else
          Lwt.return Set.empty
       | Type.Int -> asuper2_between_int e at.B.Attribute_type.at_id
       | Type.String -> asuper2_between_string e at.B.Attribute_type.at_id)

    let asuper1_search, asuper1_search_cache =
      memo_3lwt @@ fun (at_id, x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper1_search Set.add (e, at_id, x) Set.empty

    let asuper1_search_fts, asuper1_search_fts_cache =
      memo_2lwt @@ fun (x, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper1_search_fts Set.add (e, x) Set.empty

    let preimage1 p e =
      (match p with
       | B.Relation.Inter _ | B.Relation.In _ -> preimage_generic p [e]
       | B.Relation.Present at ->
          (match B.Attribute_type.value_type at with
           | Type.Bool -> asuper_present_bool e at.B.Attribute_type.at_id
           | Type.Int -> asuper_present_int e at.B.Attribute_type.at_id
           | Type.String -> asuper_present_string e at.B.Attribute_type.at_id)
       | B.Relation.Eq (at, x) -> asuper1 Q.ap1_eq at x e
       | B.Relation.Leq (at, x) -> asuper1 Q.ap1_leq at x e
       | B.Relation.Geq (at, x) -> asuper1 Q.ap1_geq at x e
       | B.Relation.Between (at, x0, x1) -> asuper2_between e at x0 x1
       | B.Relation.Search (at, x) ->
          asuper1_search (Attribute_type.id at) x e
       | B.Relation.Search_fts x -> asuper1_search_fts x e)

    let preimage1_eq at e = asuper1 Q.ap1_eq at e

    let image1_fts, image1_fts_cache =
      memo_6lwt @@ fun (et, super, cutoff, limit, fts, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let arg x = (fts, e, x, cutoff) in
      (match et, super, limit with
       | None, None, None ->
          C.fold Q.e_asub_fts List.cons (arg ()) []
       | None, None, Some lim ->
          C.fold Q.e_asub_fts_lim List.cons (arg lim) []
       | None, Some s, None ->
          C.fold Q.e_asub_fts_super List.cons (arg s) []
       | None, Some s, Some lim ->
          C.fold Q.e_asub_fts_super_lim List.cons (arg (s, lim)) []
       | Some et, None, None ->
          C.fold Q.e_asub_fts_et List.cons (arg et) []
       | Some et, None, Some lim ->
          C.fold Q.e_asub_fts_et_lim List.cons (arg (et, lim)) []
       | Some et, Some s, None ->
          C.fold Q.e_asub_fts_et_super List.cons (arg (et, s)) []
       | Some et, Some s, Some lim ->
          C.fold Q.e_asub_fts_et_super_lim List.cons (arg (et, s, lim)) [])
      >|=? List.rev

    let image1_fts ?entity_type ?super ?(cutoff = 0.0) ?limit =
      image1_fts entity_type super cutoff limit

    let preimage1_fts, preimage1_fts_cache =
      memo_6lwt @@ fun (et, super, cutoff, limit, fts, e) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let arg x = (fts, e, x, cutoff) in
      (match et, super, limit with
       | None, None, None ->
          C.fold Q.e_asuper_fts List.cons (arg ()) []
       | None, None, Some lim ->
          C.fold Q.e_asuper_fts_lim List.cons (arg lim) []
       | None, Some s, None ->
          C.fold Q.e_asuper_fts_super List.cons (arg s) []
       | None, Some s, Some lim ->
          C.fold Q.e_asuper_fts_super_lim List.cons (arg (s, lim)) []
       | Some et, None, None ->
          C.fold Q.e_asuper_fts_et List.cons (arg et) []
       | Some et, None, Some lim ->
          C.fold Q.e_asuper_fts_et_lim List.cons (arg (et, lim)) []
       | Some et, Some s, None ->
          C.fold Q.e_asuper_fts_et_super List.cons (arg (et, s)) []
       | Some et, Some s, Some lim ->
          C.fold Q.e_asuper_fts_et_super_lim List.cons (arg (et, s, lim)) [])
      >|=? List.rev

    let preimage1_fts ?entity_type ?super ?(cutoff = 0.0) ?limit =
      preimage1_fts entity_type super cutoff limit

    let mapping1_bool, mapping1_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.Bool in
        Map.add e' (Values.add v vs) m in
      C.fold Q.e_mapping1_bool aux (e, at_id) Map.empty

    let mapping1_int, mapping1_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
        Map.add e' (Values.add v vs) m in
      C.fold Q.e_mapping1_int aux (e, at_id) Map.empty

    let mapping1_string, mapping1_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
        Map.add e' (Values.add v vs) m in
      C.fold Q.e_mapping1_string aux (e, at_id) Map.empty

    let mapping1 (type a) (at : a B.Attribute_type.t) e
        : a Values.t Map.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> mapping1_bool e at.B.Attribute_type.at_id
       | Type.Int -> mapping1_int e at.B.Attribute_type.at_id
       | Type.String -> mapping1_string e at.B.Attribute_type.at_id)

    let premapping1_bool, premapping1_bool_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.Bool in
        Map.add e' (Values.add v vs) m in
      C.fold Q.e_premapping1_bool aux (e, at_id) Map.empty

    let premapping1_int, premapping1_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
        Map.add e' (Values.add v vs) m in
      C.fold Q.e_premapping1_int aux (e, at_id) Map.empty

    let premapping1_string, premapping1_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
        Map.add e' (Values.add v vs) m in
      C.fold Q.e_premapping1_string aux (e, at_id) Map.empty

    let premapping1 (type a) (at : a B.Attribute_type.t) e
        : a Values.t Map.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> premapping1_bool e at.B.Attribute_type.at_id
       | Type.Int -> premapping1_int e at.B.Attribute_type.at_id
       | Type.String -> premapping1_string e at.B.Attribute_type.at_id)

    let clear_bool_caches () =
      Cache.clear get_values_bool_cache;
      Cache.clear asub_present_bool_cache;
      Cache.clear asuper_present_bool_cache;
      Cache.clear asub1_bool_cache;
      Cache.clear asuper1_bool_cache;
      Cache.clear mapping1_bool_cache;
      Cache.clear premapping1_bool_cache

    let clear_int_caches () =
      Cache.clear get_values_int_cache;
      Cache.clear asub_present_int_cache;
      Cache.clear asuper_present_int_cache;
      Cache.clear asub1_int_cache;
      Cache.clear asuper1_int_cache;
      Cache.clear asub2_between_int_cache;
      Cache.clear asuper2_between_int_cache;
      Cache.clear mapping1_int_cache;
      Cache.clear premapping1_int_cache

    let clear_string_caches () =
      Cache.clear get_values_string_cache;
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
      Cache.clear image1_fts_cache;
      Cache.clear preimage1_fts_cache;
      Cache.clear mapping1_string_cache;
      Cache.clear premapping1_string_cache

    let clear_attr_caches (type a) (at : a B.Attribute_type.t) : unit =
      match B.Attribute_type.value_type at with
      | Type.Bool -> clear_bool_caches ()
      | Type.Int -> clear_int_caches ()
      | Type.String -> clear_string_caches ()

    (* Modifying Functions *)

    let set_rank r e =
      with_db_exn (fun (module C) -> C.exec Q.e_set_entity_rank (r, e))
      >|= fun () -> Cache.replace rank_cache fetch_grade e r

    let rec raise_rank r_min e =
      let%lwt r = rank e in
      if r >= r_min then Lwt.return_unit else
      begin
        dsub e >>= Set.iter_s (raise_rank (r_min + 1)) >>= fun () ->
        set_rank r_min e
      end

    let rec lower_rank e =
      let%lwt r = rank e in
      let update_rank eS r' =
        if r' = r then Lwt.return r' else
        rank eS >|= (max r' % succ) in
      let%lwt esS = dsuper e in
      let%lwt r' = Set.fold_s update_rank esS 0 in
      if r' = r then Lwt.return_unit else begin
        set_rank r' e >>= fun () ->
        dsub e >>= Set.iter_s begin fun eP ->
          let%lwt rP = rank eP in
          if rP = r + 1 then lower_rank eP
                        else Lwt.return_unit
        end
      end

    let force_dsub' subentity superentity (module C : CONNECTION) =
      begin
        C.exec Q.e_maybe_insert_inclusion (subentity, superentity) >>=?? fun()->
        C.exec Q.e_subsume_inclusion (subentity, superentity)
      end >|= fun result ->
      clear_inclusion_caches ();
      emit_changed subentity `Dsuper;
      emit_changed superentity `Dsub;
      result

    let relax_dsub' subentity superentity (module C : CONNECTION) =
      begin
        C.find Q.e_is_subsumed (subentity, superentity) >>=?? fun is_subsumed ->
        (if is_subsumed then Lwt.return_ok () else
         C.exec Q.e_unsubsume_inclusion (subentity, superentity)) >>=?? fun() ->
        C.exec Q.e_delete_inclusion (subentity, superentity)
      end >|= fun result ->
      clear_inclusion_caches ();
      emit_changed subentity `Dsuper;
      emit_changed superentity `Dsub;
      result

    let force_dsub subentity superentity =
      let%lwt is_super = is_sub superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      let%lwt subentity_rank = rank subentity in
      let%lwt superentity_rank = rank superentity in
      raise_rank (max subentity_rank (superentity_rank + 1)) subentity >>= fun () ->
      with_db_exn (force_dsub' subentity superentity)

    let relax_dsub subentity superentity =
      with_db_exn (relax_dsub' subentity superentity) >>= fun () ->
      let%lwt subentity_rank = rank subentity in
      let%lwt superentity_rank = rank superentity in
      if subentity_rank > superentity_rank + 1 then Lwt.return_unit
                                               else lower_rank subentity

    let check_uniqueness_for_add new_at new_avs e e' =
      let vt = B.Attribute_type.value_type new_at in
      let new_cond = B.Relation.In (new_at, Values.of_elements vt new_avs) in
      let is_violated au =
        let%lwt aff_ats =
          Attribute_uniqueness.affected au >|=
          B.Attribute_type.Set.remove (B.Attribute_type.Ex new_at) in
        try%lwt
          let%lwt conds = Lwt_list.map_s
            (fun (B.Attribute_type.Ex at) ->
              let%lwt avs = get_values at e e' in
              if Values.is_empty avs then Lwt.fail Not_found else
              Lwt.return (B.Relation.In (at, avs)))
            (B.Attribute_type.Set.elements aff_ats) in
          asub_conj e (new_cond :: conds) >|= (not % Set.is_empty)
        with Not_found ->
          Lwt.return_false in
      let%lwt violated =
        Attribute_uniqueness.affecting new_at >>=
        B.Attribute_uniqueness.Set.filter_s is_violated in
      if B.Attribute_uniqueness.Set.is_empty violated then Lwt.return_unit else
      Lwt.fail (B.Attribute_uniqueness.Not_unique violated)

    let post_attribute_update (type a) (module C : CONNECTION)
                              (at : a B.Attribute_type.t) e e' =
      clear_attr_caches at;
      emit_changed e `Asuper;
      emit_changed e' `Asub;
      (match at.B.Attribute_type.at_value_type with
       | Type.String ->
          C.exec Q.fts_clear (e, e') >>=?? fun () ->
          C.exec Q.fts_insert (e, e')
       | _ -> Lwt.return_ok ())

    let add_values' (module C : CONNECTION) (type a)
                    (at : a B.Attribute_type.t) (xs : a list) e e' =
      let at_id = at.B.Attribute_type.at_id in
      let insert_value (x : a) =
        (match at.B.Attribute_type.at_value_type with
         | Type.Bool ->
            C.exec Q.e_insert_attribution_bool (at_id, x, e, e')
         | Type.Int ->
            C.exec Q.e_insert_attribution_int (at_id, x, e, e')
         | Type.String ->
            C.exec Q.e_insert_attribution_string (at_id, x, e, e')) in
      lwt_list_iter_rs insert_value xs >>=?? fun () ->
      post_attribute_update (module C) at e e'

    let check_mult at e e' =
      let%lwt et = entity_type e in
      let%lwt et' = entity_type e' in
      match%lwt Entity_type.can_attribute at et et' with
      | false ->
        let%lwt etn = Entity_type.name et in
        let%lwt etn' = Entity_type.name et' in
        lwt_failure_f "add_values: %s is not allowed from %s to %s."
                      at.B.Attribute_type.at_name etn etn'
      | true -> Lwt.return (B.Attribute_type.value_mult at)

    let add_values (type a) (at : a B.Attribute_type.t) (xs : a Values.t) e e' =
      let xs = Values.elements xs in (* TODO: Optimise. *)
      let%lwt xs_pres = get_values at e e' in
      let%lwt xs =
        (match%lwt check_mult at e e' with
         | Multiplicity.May1 | Multiplicity.Must1 ->
            if Values.is_empty xs_pres
            then Lwt.return xs
            else lwt_failure_f "add_values: Attribute already set.";
         | Multiplicity.May | Multiplicity.Must ->
            let ht = Hashtbl.create 7 in
            Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
            let once x =
              if Hashtbl.mem ht x then false else (Hashtbl.add ht x (); true) in
            Lwt.return (List.filter once xs)) in
      if xs = [] then Lwt.return_unit else
      (* FIXME: Transaction. *)
      check_uniqueness_for_add at xs e e' >>= fun () ->
      with_db_exn ~transaction:true (fun conn -> add_values' conn at xs e e')

    let remove_values' (module C : CONNECTION) (type a)
                       (at : a B.Attribute_type.t) (xs : a list) e e' =
      let at_id = at.B.Attribute_type.at_id in
      let delete_value (x : a) =
        (match at.B.Attribute_type.at_value_type with
         | Type.Bool ->
            C.exec Q.e_delete_attribution_bool (at_id, x, e, e')
         | Type.Int ->
            C.exec Q.e_delete_attribution_int (at_id, x, e, e')
         | Type.String ->
            C.exec Q.e_delete_attribution_string (at_id, x, e, e')) in
      lwt_list_iter_rs delete_value xs >>=?? fun () ->
      post_attribute_update (module C) at e e'

    let remove_values (type a) (at : a B.Attribute_type.t)
                      (xs : a Values.t) e e' =
      let xs = Values.elements xs in (* TODO: Optimise. *)
      let%lwt xs_pres = get_values at e e' in
      let xs =
        let ht = Hashtbl.create 7 in
        Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
        List.filter
          (fun x -> if not (Hashtbl.mem ht x) then false else
                    (Hashtbl.remove ht x; true)) xs in
      if xs = [] then Lwt.return_unit else
      with_db_exn ~transaction:true (fun conn -> remove_values' conn at xs e e')

    let set_values (type a) (at : a B.Attribute_type.t) (xs : a Values.t) e e' =
      let xs = Values.elements xs in (* TODO: Optimise. *)
      begin match%lwt check_mult at e e' with
      | Multiplicity.May1 | Multiplicity.Must1 ->
        if List.length xs <= 1 then Lwt.return_unit else
        lwt_failure_f "add_values: Attribute already set.";
      | Multiplicity.May | Multiplicity.Must ->
        Lwt.return_unit
      end >>= fun () ->
      let%lwt xs_pres = get_values at e e' in
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
      (* FIXME: Transaction. *)
      check_uniqueness_for_add at xs_ins e e' >>= fun () ->
      with_db_exn ~transaction:true begin fun c ->
        (if xs_del = [] then Lwt.return_ok ()
         else remove_values' c at xs_del e e') >>=?? fun () ->
        (if xs_ins = [] then Lwt.return_ok ()
         else add_values' c at xs_ins e e')
      end

    (**/**)
    let of_id e = Lwt.return e
    let id e = e
  end
end

let connect uri =
  (module struct
    module M = Make (struct
      let wrap_transaction f (module C : CONNECTION) =
        C.start () >>=?? fun () ->
        begin try%lwt
          let%lwt r = f (module C : CONNECTION) in
          C.commit () >>=?? fun () ->
          Lwt.return r
        with exc ->
          Lwt_log.debug_f "Raised in transaction: %s" (Printexc.to_string exc)
            >>= fun () ->
          C.rollback () >>=?? fun () ->
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
    module Relation = B.Relation
    module Entity_type = struct
      include B.Entity_type
      include M.Entity_type
    end
    module Entity = struct
      include B.Entity
      include M.Entity
    end

    module type T = Subsocia_intf.S_SOID
      with type soid := int32
       and type 'a Attribute_type.t = 'a Attribute_type.t
       and type Attribute_type.ex = Attribute_type.ex
       and type Attribute_type.Set.t = Attribute_type.Set.t
       and type 'a Attribute_type.Map.t = 'a Attribute_type.Map.t
       and type Attribute_uniqueness.t = Attribute_uniqueness.t
       and type Attribute_uniqueness.Set.t = Attribute_uniqueness.Set.t
       and type 'a Attribute_uniqueness.Map.t = 'a Attribute_uniqueness.Map.t
       and type Relation.t = Relation.t
       and type Entity_type.t = Entity_type.t
       and type Entity_type.Set.t = Entity_type.Set.t
       and type 'a Entity_type.Map.t = 'a Entity_type.Map.t
       and type Entity.t = Entity.t
       and type Entity.Set.t = Entity.Set.t
       and type 'a Entity.Map.t = 'a Entity.Map.t

    let transaction f =
      M.with_db_exn @@ fun ((module C : CONNECTION) as conn) ->
      let module C' = struct
        module M = Make (struct
          let lock = Lwt_mutex.create ()
          let with_db ~transaction:_ f =
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
        module Relation = B.Relation
        module Entity_type = struct
          include B.Entity_type
          include M.Entity_type
        end
        module Entity = struct
          include B.Entity
          include M.Entity
        end
      end in
      f (module C' : T) >|= fun y -> Ok y

    let entity_changed = Int32_event_table.event Entity.changed_event_table
  end : S)
