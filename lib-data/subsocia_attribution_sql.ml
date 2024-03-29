(* Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
open Subsocia_common
open Subsocia_intf
open Unprime_list

let table_for_type : type a. a Type.t -> string = function
 | Type.Bool -> "attribution_bool"
 | Type.Int -> "attribution_int"
 | Type.String -> "attribution_string"

module type Arg = sig
  val db_schema : string option
  module Attribute_type : ATTRIBUTE_TYPE with type soid = int32
  module Relation : RELATION with module Attribute_type := Attribute_type
end

module Query = struct
  include Caqti_query
  let fL fmt = ksprintf (fun s -> L s) fmt
end

module Make (Arg : Arg) = struct
  open Arg
  open Query

  let db_schema_prefix = match db_schema with None -> "" | Some s -> s ^ "."

  type bind = Bind : int * 'a Caqti_type.t * 'a -> bind

  let table_for_adjacency = function
   | Relation.Inter _ -> assert false
   | Relation.True -> "attribution_present"
   | Relation.Present at -> table_for_type (Attribute_type.value_type at)
   | Relation.Eq (at, _) -> table_for_type (Attribute_type.value_type at)
   | Relation.In (at, _) -> table_for_type (Attribute_type.value_type at)
   | Relation.Leq (at, _) -> table_for_type (Attribute_type.value_type at)
   | Relation.Geq (at, _) -> table_for_type (Attribute_type.value_type at)
   | Relation.Between(at,_,_) -> table_for_type (Attribute_type.value_type at)
   | Relation.Search _ -> "attribution_string"
   | Relation.Search_fts _ -> "attribution_string_fts"

  let make_fromlist =
    let make_join i pred =
      fL " JOIN %s%s AS q%d \
             ON q%d.output_id = q%d.output_id \
            AND q%d.input_id = q%d.input_id"
        db_schema_prefix (table_for_adjacency pred) (i + 1) (i + 1) i (i + 1) i
    in
    (function
     | [] -> assert false
     | pred :: preds ->
        S(fL "%s%s AS q%d" db_schema_prefix (table_for_adjacency pred) 0 ::
          List.mapi make_join preds))

  let sql_of_value
      : type a. a Attribute_type.t -> a -> bind -> Query.t * bind =
    fun at x ->
    (match Attribute_type.value_type at, x with
     | Type.Bool, true -> fun bind -> (L"true", bind)
     | Type.Bool, false -> fun bind -> (L"false", bind)
     | Type.Int, i -> fun bind -> (fL "%d" i, bind)
     | Type.String, s ->
        fun (Bind (param_length, param_type, param)) ->
        let param_type = Caqti_type.(t2 param_type string) in
        let param = (param, s) in
        (P param_length, Bind (param_length + 1, param_type, param)))

  let sql_of_cond0 i at bind =
    Attribute_type.soid at >|= fun at_id ->
    let c1 = fL "q%d.attribute_type_id = %ld" i at_id in
    ([c1], bind)

  let sql_of_cond1 i op at x bind =
    Attribute_type.soid at >|= fun at_id ->
    let qx, bind = sql_of_value at x bind in
    let c1 = fL "q%d.attribute_type_id = %ld" i at_id in
    let c2 = S[fL "q%d.value %s " i op; qx] in
    ([c1; c2], bind)

  let sql_of_between i at x y bind =
    Attribute_type.soid at >|= fun at_id ->
    let qx, bind = sql_of_value at x bind in
    let qy, bind = sql_of_value at y bind in
    let c1 = fL "q%d.attribute_type_id = %ld" i at_id in
    let c2 = S[fL "q%d.value >= " i; qx] in
    let c3 = S[fL "q%d.value < " i; qy] in
    ([c1; c2; c3], bind)

  let sql_of_in i at xs bind =
    let xs = Values.elements xs in
    assert (xs <> []);
    Attribute_type.soid at >|= fun at_id ->
    let aux x (rev_qs, bind) =
      let qx, bind = sql_of_value at x bind in
      (S [fL "q%d.value = " i; qx] :: rev_qs, bind)
    in
    let rev_qs, bind = List.fold aux xs ([], bind) in
    let c1 = fL "q%d.attribute_type_id = %ld" i at_id in
    let c2 = S[L"("; concat " OR " (List.rev rev_qs); L")"] in
    ([c1; c2], bind)

  let sql_of_fts i x (Bind (param_length, param_type, param)) =
    let param_type = Caqti_type.(t2 param_type string) in
    let param = (param, x) in
    let c1 = S [
      fL "q%d.fts_vector @@ to_tsquery(q%d.fts_config::regconfig, " i i;
      P param_length; L")"
    ] in
    Lwt.return ([c1], Bind (param_length + 1, param_type, param))

  let sql_of_conjunction preds bind =
    let aux (i, rev_conds, bind) rel =
      let+ cond, bind =
        (match rel with
         | Relation.Inter _ -> assert false
         | Relation.True -> Lwt.return ([], bind)
         | Relation.Present at -> sql_of_cond0 i at bind
         | Relation.Eq (at, x) -> sql_of_cond1 i "=" at x bind
         | Relation.In (at, xs) -> sql_of_in i at xs bind
         | Relation.Leq (at, x) -> sql_of_cond1 i "<=" at x bind
         | Relation.Geq (at, x) -> sql_of_cond1 i ">=" at x bind
         | Relation.Between (at, x, y) -> sql_of_between i at x y bind
         | Relation.Search (at, x) -> sql_of_cond1 i "SIMILAR TO" at x bind
         | Relation.Search_fts x -> sql_of_fts i x bind)
      in
      (i + 1, List.rev_append cond rev_conds, bind)
    in
    Lwt_list.fold_left_s aux (0, [], bind) preds >|=
    fun (_, rev_conds, bind) -> (List.rev rev_conds, bind)

  let rec flatten = function
   | Relation.Inter ps -> List.flatten_map flatten ps
   | p -> [p]

  type request =
    | Empty
    | Request :
        ('a, int32, Caqti_mult.zero_or_more) Caqti_request.t * 'a -> request

  let select_image pred_with_inter ids =
    if ids = [] then Lwt.return Empty else
    let preds = flatten pred_with_inter in
    let* expr_conds, Bind (_, param_type, param) =
      sql_of_conjunction preds (Bind (0, Caqti_type.unit, ())) in
    let id_cond =
      S[L"("; concat " OR " (List.map (fL"q0.input_id = %ld") ids); L")"] in
    let query = S [
      L"SELECT q0.output_id";
      L" FROM "; make_fromlist preds;
      L" WHERE "; concat " AND " (id_cond :: expr_conds);
    ] in
    let request =
      Caqti_request.create ~oneshot:true
        param_type Caqti_type.int32 Caqti_mult.zero_or_more (fun _ -> query)
    in
    Lwt.return (Request (request, param))

  let select_preimage pred_with_inter ids =
    if ids = [] then Lwt.return Empty else
    let preds = flatten pred_with_inter in
    let* expr_conds, Bind (_, param_type, param) =
      sql_of_conjunction preds (Bind (0, Caqti_type.unit, ())) in
    let id_cond =
      S[L"("; concat " OR " (List.map (fL"q0.output_id = %ld") ids); L")"] in
    let query = S [
      L"SELECT q0.input_id";
      L" FROM "; make_fromlist preds;
      L" WHERE "; concat " AND " (id_cond :: expr_conds);
    ] in
    let request =
      Caqti_request.create ~oneshot:true
        param_type Caqti_type.int32 Caqti_mult.zero_or_more (fun _ -> query)
    in
    Lwt.return (Request (request, param))

end
