(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Lwt.Infix
open Lwt.Syntax
open Printf
open Scanf
open Subsocia_common
open Subsocia_prereq
open Unprime
open Unprime_list
open Unprime_option

module Log = (val Logs_lwt.src_log (Logs.Src.create "subsocia.data"))

let cache_hertz = Int64.to_float ExtUnix.Specific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz
(* let cache_section = Lwt_log.Section.make "subsocia.cache" *)

let cache_metric =
  let current_time () =
    let tms = Unix.times () in
    Unix.(tms.tms_utime +. tms.tms_stime) in
  let current_memory_pressure =
    fun () -> cache_hertz (* 1 GHz / 1 Gword *) in
(*
  let report cs =
    let open Prime_cache_metric in
    Lwt_log.ign_debug_f ~section:cache_section
      "Beacon collection: time = %g; p = %g; n_live = %d; n_dead = %d"
      cs.cs_time cs.cs_memory_pressure
      cs.cs_live_count cs.cs_dead_count in
*)
  Prime_cache_metric.create ~current_time ~current_memory_pressure ()

module Beacon = Prime_beacon.Make (struct let cache_metric = cache_metric end)

let (>|=?) = Lwt_result.(>|=)
let (>>=?) = Lwt_result.(>>=)

(* TODO: Eliminate this mixed bind, which is typically used below where the
 * right hand side throws an exception. *)
let (>>=?!) m f =
  m >>= function Ok x -> (f x >|= fun y -> Ok y) | Error _ as r -> Lwt.return r

let rec lwt_list_iter_rs f = function
 | [] -> Lwt.return_ok ()
 | x :: xs -> f x >>=? fun () -> lwt_list_iter_rs f xs

let fetch_grade = 1e-3 *. cache_second
let attribute_type_grade = fetch_grade
let preceq_grade = 1e-2 *. cache_second (* TODO: Highly non-constant. *)

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
  open Caqti_request.Infix

  let table name = Caqti_query.(S [E "."; L name])

  open Caqti_type

  (* Attribute Type *)

  let at_by_id = (int32 ->! t3 string string int)
    "SELECT attribute_name, value_type, value_mult FROM $.attribute_type \
     WHERE attribute_type_id = ?"
  let at_by_name = (string ->? t3 int32 string int)
    "SELECT attribute_type_id, value_type, value_mult FROM $.attribute_type \
     WHERE attribute_name = ?"
  let at_create = (t4 string string int (option string) ->! int32)
    "INSERT INTO $.attribute_type (attribute_name, value_type, value_mult, \
                                   fts_config) \
     VALUES (?, ?, ?, ?) RETURNING attribute_type_id"
  let at_delete = (int32 ->. unit)
    "DELETE FROM $.attribute_type WHERE attribute_type_id = ?"
  let at_all = (unit ->* int32)
    "SELECT attribute_type_id FROM $.attribute_type"

  (* Attribute Uniqueness *)

  let au_all = (unit ->* int32)
    "SELECT attribute_uniqueness_id FROM $.attribute_uniqueness"
  let au_affected = (int32 ->* int32)
    "SELECT attribute_type_id FROM $.attribute_uniqueness \
     WHERE attribute_uniqueness_id = ? ORDER BY attribute_type_id"
  let au_affecting = (int32 ->* int32)
    "SELECT attribute_uniqueness_id FROM $.attribute_uniqueness \
     WHERE attribute_type_id = ?"

  let au_force_query l =
    let open Caqti_query in
    let rec mk_values i =
      let value = S [L"("; P i; L"::int)"] in
      if i = 0 then value :: mk_values (i + 1) else
      if i < l then L", " :: value :: mk_values (i + 1) else []
    in
    let values = mk_values 0 in
    S[L"INSERT INTO "; table "attribute_uniqueness ";
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
        if l = 0 then (t ->! int32) "" else
        let q _ = au_force_query l in
        Caqti_request.create t int32 Caqti_mult.one q
      in
      let next = lazy (build (t2 t int32)) in
      Au_force_cache {request; next}
    in
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
       | id :: ids -> loop (Lazy.force next) (param, id) ids)
    in
    loop au_force_cache () at_ids

  let au_relax = (int32 ->. unit)
    "DELETE FROM $.attribute_uniqueness WHERE attribute_uniqueness_id = ?"

  (* Entity types *)

  let et_id_of_name = (string ->? int32)
    "SELECT entity_type_id FROM $.entity_type WHERE entity_type_name = ?"
  let et_name_of_id = (int32 ->! string)
    "SELECT entity_type_name FROM $.entity_type WHERE entity_type_id = ?"
  let et_create = (string ->! int32)
    "INSERT INTO $.entity_type (entity_type_name) VALUES (?) \
     RETURNING entity_type_id"
  let et_delete = (int32 ->. unit)
    "DELETE FROM $.entity_type WHERE entity_type_id = ?"
  let et_all = (unit ->* int32)
    "SELECT entity_type_id FROM $.entity_type"
  let et_entity_name_tmpl = (int32 ->! string)
    "SELECT entity_name_tmpl FROM $.entity_type WHERE entity_type_id = ?"
  let et_set_entity_name_tmpl = (t2 string int32 ->. unit)
    "UPDATE $.entity_type SET entity_name_tmpl = ? WHERE entity_type_id = ?"

  let et_can_dsub = (t2 int32 int32 ->? t2 int int)
    "SELECT dsub_mult, dsuper_mult \
     FROM $.inclusion_type \
     WHERE dsub_type_id = ? AND dsuper_type_id = ?"
  let et_dsub = (int32 ->* t3 int32 int int)
    "SELECT dsub_type_id, dsub_mult, dsuper_mult \
     FROM $.inclusion_type WHERE dsuper_type_id = ?"
  let et_dsuper = (int32 ->* t3 int32 int int)
    "SELECT dsuper_type_id, dsub_mult, dsuper_mult \
     FROM $.inclusion_type WHERE dsub_type_id = ?"
  let et_dsub_elements = (unit ->* t4 int32 int32 int int)
    "SELECT dsub_type_id, dsuper_type_id, dsub_mult, dsuper_mult \
     FROM $.inclusion_type"
  let et_allow_dsub = (t4 int int int32 int32 ->. unit)
    "INSERT INTO $.inclusion_type \
      (dsub_mult, dsuper_mult, dsub_type_id, dsuper_type_id) \
     VALUES (?, ?, ?, ?)"
  let et_disallow_dsub = (t2 int32 int32 ->. unit)
    "DELETE FROM $.inclusion_type \
     WHERE dsub_type_id = ? AND dsuper_type_id = ?"

  let et_can_attribute = (t3 int32 int32 int32 ->! int)
    "SELECT count(*) FROM $.attribution_type \
     WHERE attribute_type_id = ? AND domain_id = ? AND codomain_id = ?"
  let et_allowed_attributes = (t2 int32 int32 ->* int32)
    "SELECT attribute_type_id FROM $.attribution_type \
     WHERE domain_id = ? AND codomain_id = ?"
  let et_allowed_preimage = (int32 ->* t2 int32 int32)
    "SELECT attribute_type_id, domain_id FROM $.attribution_type \
     WHERE codomain_id = ?"
  let et_allowed_image = (int32 ->* t2 int32 int32)
    "SELECT attribute_type_id, codomain_id FROM $.attribution_type \
     WHERE domain_id = ?"
  let et_allowed_mappings = (int32 ->* t2 int32 int32)
    "SELECT domain_id, codomain_id FROM $.attribution_type \
     WHERE attribute_type_id = ?"
  let et_allowed_attributions = (unit ->* t3 int32 int32 int32)
    "SELECT attribute_type_id, domain_id, codomain_id \
     FROM $.attribution_type"
  let et_allow_attribution = (t3 int32 int32 int32 ->. unit)
    "INSERT INTO $.attribution_type \
      (attribute_type_id, domain_id, codomain_id) \
     VALUES (?, ?, ?)"
  let et_disallow_attribution = (t3 int32 int32 int32 ->. unit)
    "DELETE FROM $.attribution_type \
     WHERE attribute_type_id = ? AND domain_id = ? AND codomain_id = ?"

  (* Entites *)

  let e_is_dsub = (t3 int32 int32 ptime ->! int)
    "SELECT count(*) FROM $.inclusion \
     WHERE dsub_id = $1 AND dsuper_id = $2 \
       AND since <= $3 AND coalesce($3 < until, true)"

  let e_dsuper_any = (t2 int32 ptime ->* int32)
    "SELECT dsuper_id FROM $.inclusion \
     WHERE dsub_id = $1 \
       AND since <= $2 AND coalesce($2 < until, true)"
  let e_dsub_any = (t2 int32 ptime ->* int32)
    "SELECT dsub_id FROM $.inclusion \
     WHERE dsuper_id = $1 \
       AND since <= $2 AND coalesce($2 < until, true)"

  let e_dsuper_typed = (t3 int32 int32 ptime ->* int32)
    "SELECT entity_id \
     FROM $.inclusion JOIN $.entity ON dsuper_id = entity_id \
     WHERE dsub_id = $1 AND entity_type_id = $2 \
       AND since <= $3 AND coalesce($3 < until, true)"
  let e_dsub_typed = (t3 int32 int32 ptime ->* int32)
    "SELECT entity_id \
     FROM $.inclusion JOIN $.entity ON dsub_id = entity_id \
     WHERE dsuper_id = $1 AND entity_type_id = $2 \
       AND since <= $3 AND coalesce($3 < until, true)"

  let e_dsub_history =
    (t3 int32 (option ptime) (option ptime) ->* t3 ptime (option ptime) int32)
    "SELECT since, until, dsub_id FROM $.inclusion \
     WHERE dsuper_id = ? \
       AND coalesce (? < until, true) AND coalesce (since < ?, true) \
     ORDER BY since"

  let e_dsub_history_typed =
    (t4 int32 int32 (option ptime) (option ptime) ->*
     t3 ptime (option ptime) int32)
    "SELECT since, until, entity_id \
     FROM $.inclusion JOIN $.entity ON dsub_id = entity_id \
     WHERE dsuper_id = ? AND entity_type_id = ? \
       AND coalesce (? < until, true) AND coalesce (since < ?, true) \
     ORDER BY since"

  let e_dsuper_history =
    (t3 int32 (option ptime) (option ptime) ->* t3 ptime (option ptime) int32)
    "SELECT since, until, dsuper_id FROM $.inclusion \
     WHERE dsub_id = ? \
       AND coalesce (? < until, true) AND coalesce (since < ?, true) \
     ORDER BY since"

  let e_dsuper_history_typed =
    (t4 int32 int32 (option ptime) (option ptime) ->*
     t3 ptime (option ptime) int32)
    "SELECT since, until, entity_id \
     FROM $.inclusion JOIN $.entity ON dsuper_id = entity_id \
     WHERE dsub_id = ? AND entity_type_id = ? \
       AND coalesce (? < until, true) AND coalesce (since < ?, true) \
     ORDER BY since"

  let e_type = (int32 ->! int32)
    "SELECT entity_type_id FROM $.entity WHERE entity_id = ?"
  let e_rank = (int32 ->! int)
    "SELECT entity_rank FROM $.entity WHERE entity_id = ?"

  let e_type_members = (int32 ->* int32)
    "SELECT entity_id FROM $.entity WHERE entity_type_id = ?"

  let e_minimums = (unit ->* int32)
    "SELECT entity_id FROM $.entity \
     WHERE NOT EXISTS \
      (SELECT 0 FROM $.inclusion WHERE dsuper_id = entity_id)"

  let e_select_precedes_now = (t3 int32 int32 int ->! int)
    "WITH RECURSIVE successors(entity_id) AS ( \
        SELECT i.dsuper_id AS entity_id \
        FROM $.inclusion i \
        JOIN $.entity e ON e.entity_id = i.dsuper_id \
        WHERE i.dsub_id = $1 \
          AND i.until IS NULL AND e.entity_rank >= $3 \
      UNION \
        SELECT DISTINCT i.dsuper_id \
        FROM $.inclusion i \
        JOIN $.entity e ON e.entity_id = i.dsuper_id \
        JOIN successors c ON i.dsub_id = c.entity_id \
        WHERE i.until is NULL AND e.entity_rank >= $3 \
     ) \
     SELECT count(*) FROM successors WHERE entity_id = $2 LIMIT 1"

  let e_select_precedes_past = (t3 int32 int32 ptime ->! int)
    "WITH RECURSIVE successors(entity_id) AS ( \
        SELECT i.dsuper_id AS entity_id \
        FROM $.inclusion i \
        WHERE i.dsub_id = $1 \
          AND i.since <= $3 AND coalesce($3 < i.until, true) \
      UNION \
        SELECT DISTINCT i.dsuper_id \
        FROM $.inclusion i \
        JOIN successors c ON i.dsub_id = c.entity_id \
        WHERE i.since <= $3 AND coalesce($3 < i.until, true) \
     ) \
     SELECT count(*) FROM successors WHERE entity_id = $2 LIMIT 1"

  let e_create_entity = (int32 ->! int32)
    "INSERT INTO $.entity (entity_type_id) \
     VALUES (?) RETURNING entity_id"

  let e_delete_entity = (int32 ->. unit)
    "DELETE FROM $.entity WHERE entity_id = ?"

  let e_maybe_insert_inclusion = (t3 int32 int32 ptime ->. unit)
    "INSERT INTO $.inclusion (dsub_id, dsuper_id, since) SELECT $1, $2, $3 \
     WHERE NOT EXISTS \
      (SELECT 0 FROM $.inclusion \
       WHERE dsub_id = $1 AND dsuper_id = $2 AND coalesce($3 <= until, true))"

  let e_delete_inclusion = (t3 int32 int32 ptime ->. unit)
    "UPDATE $.inclusion SET until = $3 \
     WHERE dsub_id = $1 AND dsuper_id = $2 AND until IS NULL"

  let e_select_attribution_bool = (t3 int32 int32 int32 ->* bool)
    "SELECT value FROM $.attribution_bool \
     WHERE input_id = ? AND output_id = ? AND attribute_type_id = ?"
  let e_select_attribution_int = (t3 int32 int32 int32 ->* int)
    "SELECT value FROM $.attribution_int \
     WHERE input_id = ? AND output_id = ? AND attribute_type_id = ?"
  let e_select_attribution_string = (t3 int32 int32 int32 ->* string)
    "SELECT value FROM $.attribution_string \
     WHERE input_id = ? AND output_id = ? AND attribute_type_id = ?"

  let e_insert_attribution_bool = (t4 int32 bool int32 int32 ->. unit)
    "INSERT INTO $.attribution_bool \
      (attribute_type_id, value, input_id, output_id) \
     VALUES (?, ?, ?, ?)"
  let e_insert_attribution_int = (t4 int32 int int32 int32 ->. unit)
    "INSERT INTO $.attribution_int \
      (attribute_type_id, value, input_id, output_id) \
     VALUES (?, ?, ?, ?)"
  let e_insert_attribution_string = (t4 int32 string int32 int32 ->. unit)
    "INSERT INTO $.attribution_string \
      (attribute_type_id, value, input_id, output_id) \
     VALUES (?, ?, ?, ?)"

  let e_delete_attribution_bool = (t4 int32 bool int32 int32 ->. unit)
    "DELETE FROM $.attribution_bool \
     WHERE attribute_type_id = ? AND value = ? \
       AND input_id = ? AND output_id = ?"
  let e_delete_attribution_int = (t4 int32 int int32 int32 ->. unit)
    "DELETE FROM $.attribution_int \
     WHERE attribute_type_id = ? AND value = ? \
       AND input_id = ? AND output_id = ?"
  let e_delete_attribution_string = (t4 int32 string int32 int32 ->. unit)
    "DELETE FROM $.attribution_string \
     WHERE attribute_type_id = ? AND value = ? \
       AND input_id = ? AND output_id = ?"

  let ap1_ops = [|"="; "<="; ">="|]
  let ap1_eq = 0
  let ap1_leq = 1
  let ap1_geq = 2

  let e_asub_present_any = (int32 ->* int32)
    "SELECT output_id FROM $.attribution_present WHERE input_id = ?"
  let e_asub_present_bool = (t2 int32 int32 ->* int32)
    "SELECT output_id FROM $.attribution_bool \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_asub_present_int = (t2 int32 int32 ->* int32)
    "SELECT output_id FROM $.attribution_int \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_asub_present_string = (t2 int32 int32 ->* int32)
    "SELECT output_id FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ?"

  let e_asuper_present_any = (int32 ->* int32)
    "SELECT input_id FROM $.attribution_present WHERE output_id = ?"
  let e_asuper_present_bool = (t2 int32 int32 ->* int32)
    "SELECT input_id FROM $.attribution_bool \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_asuper_present_int = (t2 int32 int32 ->* int32)
    "SELECT input_id FROM $.attribution_int \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_asuper_present_string = (t2 int32 int32 ->* int32)
    "SELECT input_id FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ?"

  let e_asub1_bool = ap1_ops |> Array.map @@ fun op ->
    (t3 int32 int32 bool ->* int32)
    ("SELECT output_id FROM $.attribution_bool \
      WHERE input_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asub1_int = ap1_ops |> Array.map @@ fun op ->
    (t3 int32 int32 int ->* int32)
    ("SELECT output_id FROM $.attribution_int \
      WHERE input_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asub1_string = ap1_ops |> Array.map @@ fun op ->
    (t3 int32 int32 string ->* int32)
    ("SELECT output_id FROM $.attribution_string \
      WHERE input_id = ? AND attribute_type_id = ? AND value "^op^" ?")

  let e_asuper1_bool = ap1_ops |> Array.map @@ fun op ->
    (t3 int32 int32 bool ->* int32)
    ("SELECT input_id FROM $.attribution_bool \
      WHERE output_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asuper1_int = ap1_ops |> Array.map @@ fun op ->
    (t3 int32 int32 int ->* int32)
    ("SELECT input_id FROM $.attribution_int \
      WHERE output_id = ? AND attribute_type_id = ? AND value "^op^" ?")
  let e_asuper1_string = ap1_ops |> Array.map @@ fun op ->
    (t3 int32 int32 string ->* int32)
    ("SELECT input_id FROM $.attribution_string \
      WHERE output_id = ? AND attribute_type_id = ? AND value "^op^" ?")

  let e_asub2_between_int = (t4 int32 int32 int int ->* int32)
    "SELECT output_id FROM $.attribution_int \
     WHERE input_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"
  let e_asub2_between_string = (t4 int32 int32 string string ->* int32)
    "SELECT output_id FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"

  let e_asuper2_between_int = (t4 int32 int32 int int ->* int32)
    "SELECT input_id FROM $.attribution_int \
     WHERE output_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"
  let e_asuper2_between_string = (t4 int32 int32 string string ->* int32)
    "SELECT input_id FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ? \
       AND value >= ? AND value < ?"

  let e_asub1_search = (t3 int32 int32 string ->* int32)
    "SELECT output_id FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ? \
       AND value SIMILAR TO ?"
  let e_asuper1_search = (t3 int32 int32 string ->* int32)
    "SELECT input_id FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ? \
       AND value SIMILAR TO ?"

  let e_asub1_search_fts = (t2 int32 string ->* int32)
    "SELECT output_id FROM $.attribution_string_fts \
     WHERE input_id = ? \
       AND fts_vector @@ to_tsquery(fts_config::regconfig, ?)"
  let e_asuper1_search_fts = (t2 int32 string ->* int32)
    "SELECT input_id FROM $.attribution_string_fts \
     WHERE output_id = ? \
       AND fts_vector @@ to_tsquery(fts_config::regconfig, ?)"

  let _asub_fts with_et with_super with_lim tA =
    (t4 string int32 tA float ->* t2 int32 float) @@
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
    (t4 string int32 tA float ->* t2 int32 float) @@
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
  let e_asub_fts_super_lim      = _asub_fts   false true  true  (t2 int32 int)
  let e_asuper_fts_super_lim    = _asuper_fts false true  true  (t2 int32 int)
  let e_asub_fts_et             = _asub_fts   true  false false int32
  let e_asuper_fts_et           = _asuper_fts true  false false int32
  let e_asub_fts_et_lim         = _asub_fts   true  false true  (t2 int32 int)
  let e_asuper_fts_et_lim       = _asuper_fts true  false true  (t2 int32 int)
  let e_asub_fts_et_super       = _asub_fts   true  true  false (t2 int32 int32)
  let e_asuper_fts_et_super     = _asuper_fts true  true  false (t2 int32 int32)
  let e_asub_fts_et_super_lim   = _asub_fts   true  true  true  (t3 int32 int32 int)
  let e_asuper_fts_et_super_lim = _asuper_fts true  true  true  (t3 int32 int32 int)

  let e_mapping1_bool = (t2 int32 int32 ->* t2 int32 bool)
    "SELECT output_id, value FROM $.attribution_bool \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_mapping1_int = (t2 int32 int32 ->* t2 int32 int)
    "SELECT output_id, value FROM $.attribution_int \
     WHERE input_id = ? AND attribute_type_id = ?"
  let e_mapping1_string = (t2 int32 int32 ->* t2 int32 string)
    "SELECT output_id, value FROM $.attribution_string \
     WHERE input_id = ? AND attribute_type_id = ?"

  let e_premapping1_bool = (t2 int32 int32 ->* t2 int32 bool)
    "SELECT input_id, value FROM $.attribution_bool \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_premapping1_int = (t2 int32 int32 ->* t2 int32 int)
    "SELECT input_id, value FROM $.attribution_int \
     WHERE output_id = ? AND attribute_type_id = ?"
  let e_premapping1_string = (t2 int32 int32 ->* t2 int32 string)
    "SELECT input_id, value FROM $.attribution_string \
     WHERE output_id = ? AND attribute_type_id = ?"

  let e_connected_by_bool = (t2 int32 bool ->* t2 int32 int32)
    "SELECT input_id, output_id FROM $.attribution_bool \
     WHERE attribute_type = ? AND value = ?"
  let e_connected_by_int = (t2 int32 int ->* t2 int32 int32)
    "SELECT input_id, output_id FROM $.attribution_int \
     WHERE attribute_type = ? AND value = ?"
  let e_connected_by_string = (t2 int32 string ->* t2 int32 int32)
    "SELECT input_id, output_id FROM $.attribution_string \
     WHERE attribute_type = ? AND value = ?"

  let fts_clear = (t2 int32 int32 ->. unit)
    "DELETE FROM $.attribution_string_fts \
     WHERE input_id = ? AND output_id = ?"
  let fts_insert = (t2 int32 int32 ->. unit)
    "INSERT INTO $.attribution_string_fts \
                  (input_id, output_id, fts_config, fts_vector) \
     SELECT a.input_id, a.output_id, at.fts_config, \
            to_tsvector(at.fts_config::regconfig, string_agg(value, '$')) \
     FROM $.attribution_string AS a \
       NATURAL JOIN $.attribute_type AS at \
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
        let* y = f x in
        Prime_cache.replace cache fetch_grade x y;
        Lwt.return y
    in
    (g, cache)

  let memo_lwt_conn f =
    let cache = Prime_cache.create ~cache_metric 23 in
    let g ?conn x =
      try Lwt.return (Prime_cache.find cache x)
      with Not_found ->
        let* y = f ?conn x in
        Prime_cache.replace cache fetch_grade x y;
        Lwt.return y
    in
    (g, cache)
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
  (val if Subsocia_config.(global.enable_caching)
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

let memo_t1lwt f =
  let g, c = Cache.memo_lwt (fun x -> f ~time:(Ptime_clock.now ()) x) in
  let h ?time x = match time with None -> g x | Some time -> f ~time x in
  (h, c)

let memo_t2lwt f =
  let g, c = memo_t1lwt f in
  (fun ?time x0 x1 -> g ?time (x0, x1)), c

module B = struct
  module Attribute_type = struct
    type soid = int32
    type 'a t = {
      at_id : int32;
      at_name : string;
      at_value_type : 'a Type.t;
      at_value_mult : Multiplicity.t;
      at_beacon : Beacon.t;
    } [@@warning "-69"]
    type ex = Ex : 'a t -> ex
    type any = Any : 'a t -> any

    let value_type at = at.at_value_type
    let value_mult at = at.at_value_mult

    let coerce_any (type a) (t : a Type.t) at0 : a t option =
      let Any at1 = at0 in
      (match t, value_type at1, at1 with
       | Type.Bool, Type.Bool, at -> Some at
       | Type.Bool, _, _ -> None
       | Type.Int, Type.Int, at -> Some at
       | Type.Int, _, _ -> None
       | Type.String, Type.String, at -> Some at
       | Type.String, _, _ -> None)

    module Comparable = struct
      type t = any
      let compare (Any x) (Any y) = compare x.at_id y.at_id
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
      | True : t
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

  val db_schema : string option

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
        Log.debug (fun f -> f "%a" Caqti_error.pp err) >>= fun () ->
        Lwt.fail (Caqti_error.Exn err))

  module Attribute_type = struct
    open B.Attribute_type

    let soid at = Lwt.return at.at_id
    let name at = Lwt.return at.at_name

    let any_of_soid_exn', any_of_soid_exn_cache = Cache.memo_lwt_conn @@
      fun ?conn at_id ->
        with_db_exn ?conn @@ fun (module C : CONNECTION) ->
        C.find Q.at_by_id at_id >|=? fun (at_name, value_type, value_mult) ->
        let Type.Any at_value_type = Type.any_of_string value_type in
        let at_value_mult = Multiplicity.of_int value_mult in
        Beacon.embed attribute_type_grade @@ fun at_beacon ->
        Any {at_id; at_name; at_value_type; at_value_mult; at_beacon}

    let of_soid_exn' ?conn vt' id =
      any_of_soid_exn' ?conn id >>= fun at ->
      (match coerce_any vt' at with
       | Some at -> Lwt.return at
       | None ->
          let Any at = at in
          let vt, an = at.at_value_type, at.at_name in
          let err = `Attribute_type_mismatch (an, Type.Any vt, Type.Any vt') in
          Lwt.fail (Subsocia_error.Exn err))

    let of_soid_exn id = of_soid_exn' id
    let any_of_soid_exn id = any_of_soid_exn' id

    let any_of_name, any_of_name_cache = memo_1lwt @@ fun at_name ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.find_opt Q.at_by_name at_name >|=?
      (function
       | None -> Error (`Attribute_type_missing at_name)
       | Some (at_id, value_type, value_mult) ->
          let Type.Any at_value_type = Type.any_of_string value_type in
          let at_value_mult = Multiplicity.of_int value_mult in
          Beacon.embed attribute_type_grade @@ fun at_beacon ->
          Ok (Any {at_id; at_name; at_value_type; at_value_mult; at_beacon}))

    let any_of_name_exn name =
      any_of_name name >>=
      (function
       | Ok any -> Lwt.return any
       | Error (`Attribute_type_missing _ as err) ->
          Lwt.fail (Subsocia_error.Exn err))

    let of_name_exn vt' name =
      any_of_name_exn name >>= fun at ->
      (match coerce_any vt' at with
       | Some at -> Lwt.return at
       | None ->
          let Any at = at in
          let vt, an = at.at_value_type, at.at_name in
          let err = `Attribute_type_mismatch (an, Type.Any vt, Type.Any vt') in
          Lwt.fail (Subsocia_error.Exn err))

    let create
        : type a. ?mult: Multiplicity.t -> a Type.t -> string -> a t Lwt.t =
      fun ?(mult = Multiplicity.May) vt at_name ->
      let fts =
        (match vt with
         | Type.String ->
            let len = String.length at_name in
            if len < 3 || at_name.[len - 3] <> '.' then Some "simple" else
            Some (tsconfig_of_lang2 (String.sub at_name (len - 2) 2))
         | _ -> None)
      in
      with_db_exn @@ fun ((module C : CONNECTION) as conn) ->
      C.find Q.at_create
        (at_name, Type.to_string vt, Multiplicity.to_int mult, fts)
        >>=?! of_soid_exn' ~conn vt

    let delete at =
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.exec Q.at_delete at.at_id

    let all () =
      with_db_exn @@ fun ((module C : CONNECTION) as conn) ->
      C.fold Q.at_all List.cons () [] >>=?! fun at_ids ->
      Lwt_list.fold_s
        (fun id acc -> any_of_soid_exn' ~conn id >|= fun at -> Set.add at acc)
        at_ids Set.empty

    let clear_caches () =
      Cache.clear any_of_soid_exn_cache;
      Cache.clear any_of_name_cache

    let id at = at.at_id
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
      >>= Lwt_list.rev_map_s Attribute_type.any_of_soid_exn
      >|= B.Attribute_type.Set.of_ordered_elements

    let find atset =
      let B.Attribute_type.Any at = B.Attribute_type.Set.min_elt_exn atset in
      affecting at >>=
      Set.filter_s
        (fun au -> affected au >|= B.Attribute_type.Set.equal atset)
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
          (List.map (fun (B.Attribute_type.Any at) -> Attribute_type.id at) ats)
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

    let clear_caches () =
      Cache.clear all_cache;
      Cache.clear affecting_cache;
      Cache.clear affected_cache
  end

  module Attribution_sql = Subsocia_attribution_sql.Make (struct
    let db_schema = P.db_schema
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
    let of_name_exn name =
      of_name name >>= function
       | Some et -> Lwt.return et
       | None -> Lwt.fail (Subsocia_error.Exn (`Entity_type_missing name))

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
        let* at = Attribute_type.any_of_soid_exn' ~conn at_id in
        Lwt.return (B.Attribute_type.Set.add at at_map)
      in
      C.fold Q.et_allowed_attributes List.cons (et, et') []
        >>=?! fun at_ids ->
      Lwt_list.fold_left_s aux B.Attribute_type.Set.empty at_ids

    let allowed_preimage, allowed_preimage_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun ((module C) as conn) ->
      let aux acc (at_id, et) =
        let* at = Attribute_type.any_of_soid_exn' ~conn at_id in
        let ats' = try Map.find et acc with Not_found -> [] in
        Lwt.return (Map.add et (at :: ats') acc)
      in
      C.fold Q.et_allowed_preimage List.cons et [] >>=?! fun bindings ->
      Lwt_list.fold_left_s aux Map.empty bindings

    let allowed_image, allowed_image_cache =
      memo_1lwt @@ fun et ->
      with_db_exn @@ fun ((module C) as conn) ->
      let aux acc (at_id, et) =
        let* at = Attribute_type.any_of_soid_exn' ~conn at_id in
        let ats' = try Map.find et acc with Not_found -> [] in
        Lwt.return (Map.add et (at :: ats') acc)
      in
      C.fold Q.et_allowed_image List.cons et [] >>=?! fun bindings ->
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
        Attribute_type.any_of_soid_exn' ~conn at_id >|= fun at ->
        (at, et0, et1)
      in
      C.fold Q.et_allowed_attributions List.cons () [] >>=?! Lwt_list.map_s aux

    let allow_attribution at et et' =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_allow_attribution (at.B.Attribute_type.at_id, et, et')

    let disallow_attribution at et et' =
      with_db_exn @@ fun (module C) ->
      C.exec Q.et_disallow_attribution (at.B.Attribute_type.at_id, et, et')

    let clear_caches () =
      Cache.clear of_name_cache;
      Cache.clear name_cache;
      Cache.clear entity_name_tmpl_cache;
      Cache.clear can_dsub_cache;
      Cache.clear dsub_cache;
      Cache.clear dsuper_cache;
      Cache.clear allowed_attributes_cache;
      Cache.clear allowed_preimage_cache;
      Cache.clear allowed_image_cache;
      Cache.clear allowed_mappings_cache;
      Cache.clear can_attribute_cache
  end

  module Entity = struct
    open B.Entity

    let listeners = ref []
    let on_change f = listeners := f :: !listeners
    let emit_changed msg = List.iter (fun f -> f msg) !listeners

    let compare = Int32.compare

    let of_soid = Lwt.return
    let soid = Lwt.return

    let root_id = 1l
    let is_root e = Lwt.return (e = root_id)
    let root = of_soid root_id
    let get_root () = root

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

    let dsub_any, dsub_any_cache = memo_t1lwt @@ fun ~time e ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsub_any Set.add (e, time) Set.empty

    let dsuper_any, dsuper_any_cache = memo_t1lwt @@ fun ~time e ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsuper_any Set.add (e, time) Set.empty

    let dsub_typed, dsub_typed_cache = memo_t2lwt @@ fun ~time (et, e) ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsub_typed Set.add (e, et, time) Set.empty

    let dsuper_typed, dsuper_typed_cache = memo_t2lwt @@ fun ~time (et, e) ->
      with_db_exn @@ fun (module C) ->
      C.fold Q.e_dsuper_typed Set.add (e, et, time) Set.empty

    let dsub ?time ?et e =
      (match et with
       | None -> dsub_any ?time e
       | Some et -> dsub_typed ?time et e)

    let dsuper ?time ?et e =
      (match et with
       | None -> dsuper_any ?time e
       | Some et -> dsuper_typed ?time et e)

    let dsub_history ?since ?until ?et e =
      with_db_exn @@ fun (module C) ->
      (match et with
       | None ->
          C.collect_list Q.e_dsub_history (e, since, until)
       | Some et ->
          C.collect_list Q.e_dsub_history_typed (e, et, since, until))

    let dsuper_history ?since ?until ?et e =
      with_db_exn @@ fun (module C) ->
      (match et with
       | None ->
          C.collect_list Q.e_dsuper_history (e, since, until)
       | Some et ->
          C.collect_list Q.e_dsuper_history_typed (e, et, since, until))

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

    let is_dsub, is_dsub_cache = memo_t2lwt @@ fun ~time (e, e') ->
      with_db_exn @@ fun (module C) ->
      C.find Q.e_is_dsub (e, e', time) >|=? bool_of_int

    let is_sub ?time subentity superentity =
      if subentity = superentity then Lwt.return_true else
      (match time with
       | Some time ->
          with_db_exn begin fun (module C) ->
            C.find Q.e_select_precedes_past (subentity, superentity, time)
              >|=? bool_of_int
          end
       | None ->
          let k = subentity, superentity in
          try Lwt.return (Cache.find inclusion_cache k)
          with Not_found ->
            let* r_lim = rank superentity in
            let* c = with_db_exn @@ fun (module C) ->
              C.find Q.e_select_precedes_now (subentity, superentity, r_lim)
                >|=? bool_of_int
            in
            Cache.replace inclusion_cache preceq_grade k c;
            Lwt.return c)

    let clear_misc_caches () =
      Cache.clear entity_type_cache;
      Cache.clear rank_cache;
      Cache.clear type_members_cache

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
      Attribution_sql.select_image p es >>= function
       | Attribution_sql.Empty -> Lwt.return Set.empty
       | Attribution_sql.Request (request, param) ->
          with_db_exn @@ fun (module C : CONNECTION) ->
          C.fold request Set.add param Set.empty

    (* TODO: Cache? *)
    let preimage_generic p es =
      Attribution_sql.select_preimage p es >>= function
       | Attribution_sql.Empty -> Lwt.return Set.empty
       | Attribution_sql.Request (request, param) ->
          with_db_exn @@ fun (module C : CONNECTION) ->
          C.fold request Set.add param Set.empty

    let asub_conj e ps = image_generic (B.Relation.Inter ps) [e]
    (* let asuper_conj e ps = preimage_generic (B.Relation.Inter ps) [e] *)

    let asub_present_any, asub_present_any_cache =
      memo_1lwt @@ fun e ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asub_present_any Set.add e Set.empty

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
       | B.Relation.True -> asub_present_any e
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

    let asuper_present_any, asuper_present_any_cache =
      memo_1lwt @@ fun e ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.fold Q.e_asuper_present_any Set.add e Set.empty

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
       | B.Relation.True -> asuper_present_any e
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
        Map.add e' (Values.add v vs) m
      in
      C.fold Q.e_mapping1_bool aux (e, at_id) Map.empty

    let mapping1_int, mapping1_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
        Map.add e' (Values.add v vs) m
      in
      C.fold Q.e_mapping1_int aux (e, at_id) Map.empty

    let mapping1_string, mapping1_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
        Map.add e' (Values.add v vs) m
      in
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
        Map.add e' (Values.add v vs) m
      in
      C.fold Q.e_premapping1_bool aux (e, at_id) Map.empty

    let premapping1_int, premapping1_int_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.Int in
        Map.add e' (Values.add v vs) m
      in
      C.fold Q.e_premapping1_int aux (e, at_id) Map.empty

    let premapping1_string, premapping1_string_cache =
      memo_2lwt @@ fun (e, at_id) ->
      with_db_exn @@ fun (module C : CONNECTION) ->
      let aux (e', v) m =
        let vs = try Map.find e' m with Not_found -> Values.empty Type.String in
        Map.add e' (Values.add v vs) m
      in
      C.fold Q.e_premapping1_string aux (e, at_id) Map.empty

    let premapping1 (type a) (at : a B.Attribute_type.t) e
        : a Values.t Map.t Lwt.t =
      (match B.Attribute_type.value_type at with
       | Type.Bool -> premapping1_bool e at.B.Attribute_type.at_id
       | Type.Int -> premapping1_int e at.B.Attribute_type.at_id
       | Type.String -> premapping1_string e at.B.Attribute_type.at_id)

    let connected_by_bool at_id v =
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.collect_list Q.e_connected_by_bool (at_id, v)

    let connected_by_int at_id v =
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.collect_list Q.e_connected_by_int (at_id, v)

    let connected_by_string at_id v =
      with_db_exn @@ fun (module C : CONNECTION) ->
      C.collect_list Q.e_connected_by_string (at_id, v)

    let connected_by (type a) (at : a B.Attribute_type.t) (v : a) =
      (match B.Attribute_type.value_type at with
       | Type.Bool ->
          connected_by_bool at.B.Attribute_type.at_id v
       | Type.Int ->
          connected_by_int at.B.Attribute_type.at_id v
       | Type.String ->
          connected_by_string at.B.Attribute_type.at_id v)

    let clear_poly_caches () =
      Cache.clear asub_present_any_cache;
      Cache.clear asuper_present_any_cache

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
      | Type.Bool -> clear_bool_caches (); clear_poly_caches ()
      | Type.Int -> clear_int_caches (); clear_poly_caches ()
      | Type.String -> clear_string_caches (); clear_poly_caches ()

    (* Modifying Functions *)

    let force_dsub' ?time subentity superentity (module C : CONNECTION) =
      let time = match time with Some t -> t | None -> Ptime_clock.now () in
      C.exec Q.e_maybe_insert_inclusion (subentity, superentity, time)
        >|= fun result ->
      clear_inclusion_caches ();
      emit_changed (`Force_dsub (subentity, superentity));
      result

    let relax_dsub' ?time subentity superentity (module C : CONNECTION) =
      let time = match time with Some t -> t | None -> Ptime_clock.now () in
      C.exec Q.e_delete_inclusion (subentity, superentity, time)
        >|= fun result ->
      clear_inclusion_caches ();
      emit_changed (`Relax_dsub (subentity, superentity));
      result

    let force_dsub ?time subentity superentity =
      let* is_super = is_sub ?time superentity subentity in
      if is_super then Lwt.fail (Invalid_argument "cyclic constraint") else
      with_db_exn (force_dsub' ?time subentity superentity)

    let relax_dsub ?time subentity superentity =
      with_db_exn (relax_dsub' ?time subentity superentity)

    let check_uniqueness_for_add new_at new_avs e e' =
      let vt = B.Attribute_type.value_type new_at in
      let new_cond = B.Relation.In (new_at, Values.of_elements vt new_avs) in
      let is_violated au =
        let* aff_ats =
          Attribute_uniqueness.affected au >|=
          B.Attribute_type.Set.remove (B.Attribute_type.Any new_at)
        in
        Lwt.catch
          (fun () ->
            let* conds = Lwt_list.map_s
              (fun (B.Attribute_type.Any at) ->
                let* avs = get_values at e e' in
                if Values.is_empty avs then Lwt.fail Not_found else
                Lwt.return (B.Relation.In (at, avs)))
              (B.Attribute_type.Set.elements aff_ats) in
            asub_conj e (new_cond :: conds) >|= (not % Set.is_empty))
          (function Not_found -> Lwt.return_false | exn -> Lwt.fail exn)
      in
      let* violated =
        Attribute_uniqueness.affecting new_at >>=
        B.Attribute_uniqueness.Set.filter_s is_violated
      in
      if B.Attribute_uniqueness.Set.is_empty violated then Lwt.return_unit else
      Lwt.fail (B.Attribute_uniqueness.Not_unique violated)

    let post_attribute_update (type a) (module C : CONNECTION)
                              (at : a B.Attribute_type.t) e e' =
      clear_attr_caches at;
      emit_changed (`Change_values (e, e'));
      (match at.B.Attribute_type.at_value_type with
       | Type.String ->
          C.exec Q.fts_clear (e, e') >>=? fun () ->
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
            C.exec Q.e_insert_attribution_string (at_id, x, e, e'))
      in
      lwt_list_iter_rs insert_value xs >>=? fun () ->
      post_attribute_update (module C) at e e'

    let check_mult at e e' =
      let* et = entity_type e in
      let* et' = entity_type e' in
      Entity_type.can_attribute at et et' >>= function
       | false ->
          let* etn = Entity_type.name et in
          let* etn' = Entity_type.name et' in
          Subsocia_error.fail_lwt "add_values: %s is not allowed from %s to %s."
            at.B.Attribute_type.at_name etn etn'
       | true -> Lwt.return (B.Attribute_type.value_mult at)

    let add_values (type a) (at : a B.Attribute_type.t) (xs : a Values.t) e e' =
      let xs = Values.elements xs in (* TODO: Optimise. *)
      let* xs_pres = get_values at e e' in
      let* xs =
        check_mult at e e' >>= function
         | Multiplicity.May1 | Multiplicity.Must1 ->
            if Values.is_empty xs_pres
            then Lwt.return xs
            else Subsocia_error.fail_lwt "add_values: Attribute already set.";
         | Multiplicity.May | Multiplicity.Must ->
            let ht = Hashtbl.create 7 in
            Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
            let once x =
              if Hashtbl.mem ht x then false else (Hashtbl.add ht x (); true) in
            Lwt.return (List.filter once xs)
      in
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
            C.exec Q.e_delete_attribution_string (at_id, x, e, e'))
      in
      lwt_list_iter_rs delete_value xs >>=? fun () ->
      post_attribute_update (module C) at e e'

    let remove_values (type a) (at : a B.Attribute_type.t)
                      (xs : a Values.t) e e' =
      let xs = Values.elements xs in (* TODO: Optimise. *)
      let* xs_pres = get_values at e e' in
      let xs =
        let ht = Hashtbl.create 7 in
        Values.iter (fun x -> Hashtbl.add ht x ()) xs_pres;
        List.filter
          (fun x -> if not (Hashtbl.mem ht x) then false else
                    (Hashtbl.remove ht x; true)) xs
      in
      if xs = [] then Lwt.return_unit else
      with_db_exn ~transaction:true (fun conn -> remove_values' conn at xs e e')

    let set_values (type a) (at : a B.Attribute_type.t) (xs : a Values.t) e e' =
      let xs = Values.elements xs in (* TODO: Optimise. *)
      (check_mult at e e' >>= function
       | Multiplicity.May1 | Multiplicity.Must1 ->
          if List.length xs <= 1 then Lwt.return_unit else
          Subsocia_error.fail_lwt "add_values: Attribute already set.";
       | Multiplicity.May | Multiplicity.Must ->
          Lwt.return_unit)
      >>= fun () ->
      let* xs_pres = get_values at e e' in
      let ht = Hashtbl.create 7 in
      Values.iter (fun x -> Hashtbl.add ht x false) xs_pres;
      let xs_ins = xs |> List.filter @@ fun x ->
        let pres = Hashtbl.mem ht x in
        Hashtbl.replace ht x true;
        not pres
      in
      let xs_del =
        Hashtbl.fold (fun x keep acc -> if keep then acc else x :: acc) ht [] in
      (* FIXME: Transaction. *)
      check_uniqueness_for_add at xs_ins e e' >>= fun () ->
      with_db_exn ~transaction:true begin fun c ->
        (if xs_del = [] then Lwt.return_ok () else
         remove_values' c at xs_del e e') >>=? fun () ->
        (if xs_ins = [] then Lwt.return_ok () else
         add_values' c at xs_ins e e')
      end

    let clear_caches () =
      clear_misc_caches ();
      clear_inclusion_caches ();
      clear_bool_caches ();
      clear_int_caches ();
      clear_string_caches ()
  end

  let clear_caches () =
    Attribute_type.clear_caches ();
    Attribute_uniqueness.clear_caches ();
    Entity_type.clear_caches ();
    Entity.clear_caches ()
end

let connect db_uri =
  (module struct

    let db_uri, db_schema =
      (match Uri.get_query_param db_uri "schema" with
       | None -> db_uri, Some "subsocia"
       | Some ""     -> Uri.remove_query_param db_uri "schema", None
       | Some schema -> Uri.remove_query_param db_uri "schema", Some schema)

    let db_schema_prefix = match db_schema with None -> "" | Some s -> s ^ "."

    let env di = function
     | "." ->
        (match Caqti_driver_info.dialect_tag di with
         | `Pgsql -> Caqti_query.L db_schema_prefix
         | _ -> Caqti_query.S [])
     | _ -> raise Not_found

    module M = Make (struct

      let db_schema = db_schema

      let wrap_transaction f ((module C : CONNECTION) as c) =
        C.with_transaction (fun () -> f c)

      let pool = Caqti_lwt_unix.connect_pool ~env db_uri
        |> Result.fold ~ok:Fun.id ~error:(fun e -> raise (Caqti_error.Exn e))

      let with_db ~transaction f =
        if transaction
        then Caqti_lwt_unix.Pool.use (wrap_transaction f) pool
        else Caqti_lwt_unix.Pool.use f pool
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
    let clear_caches = M.clear_caches

    module type T = Subsocia_intf.S_SOID
      with type soid := int32
       and type 'a Attribute_type.t = 'a Attribute_type.t
       and type Attribute_type.any = Attribute_type.any
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
      M.with_db_exn ~transaction:true @@ fun ((module C : CONNECTION) as conn) ->
      let module C' = struct
        module M = Make (struct
          let db_schema = db_schema
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
      f (module C' : T) >|= fun y -> clear_caches (); Ok y

    type entity_change =
      [ `Force_dsub of Entity.t * Entity.t
      | `Relax_dsub of Entity.t * Entity.t
      | `Change_values of Entity.t * Entity.t ]

    let on_entity_change = Entity.on_change

  end : S) [@@ocaml.warning "-3"]
