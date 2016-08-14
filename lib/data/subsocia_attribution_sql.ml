(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Caqti_sql
open Caqti_query
open Lwt.Infix
open Printf
open Subsocia_common
open Subsocia_intf
open Unprime_list

let table_for_type : type a. a Type.t -> string = function
  | Type.Bool -> "attribution_bool"
  | Type.Int -> "attribution_int"
  | Type.String -> "attribution_string"

let schema_prefix = ref "subsocia." (* FIXME: import *)

module type Arg = sig
  module Attribute_type : ATTRIBUTE_TYPE
    with type soid = int32
  module Relation : RELATION
    with module Attribute_type := Attribute_type
end

module Make (Arg : Arg) = struct
  open Arg

  let table_for_adjacency = function
    | Relation.Inter _ -> assert false
    | Relation.Present at -> table_for_type (Attribute_type.value_type at)
    | Relation.Eq (at, _) -> table_for_type (Attribute_type.value_type at)
    | Relation.In (at, _) -> table_for_type (Attribute_type.value_type at)
    | Relation.Leq (at, _) -> table_for_type (Attribute_type.value_type at)
    | Relation.Geq (at, _) -> table_for_type (Attribute_type.value_type at)
    | Relation.Between(at,_,_) -> table_for_type (Attribute_type.value_type at)
    | Relation.Search _ -> "attribution_string"
    | Relation.Search_fts _ -> "attribution_string_fts"

  let bprint_adjacency_conj buf preds =

    let do_join i pred =
      let tn = table_for_adjacency pred in
      if i = 0 then
        bprintf buf "%s%s q%d" !schema_prefix tn i
      else
        bprintf buf " JOIN %s%s q%d \
                        ON q%d.output_id = q%d.output_id \
                       AND q%d.input_id = q%d.input_id"
                !schema_prefix tn i i (i - 1) i (i - 1) in

    let do_value : type a. a Attribute_type.t -> a -> unit = fun at x ->
      match Attribute_type.value_type at, x with
      | Type.Bool, true -> Buffer.add_string buf "true"
      | Type.Bool, false -> Buffer.add_string buf "false"
      | Type.Int, i -> bprintf buf "%d" i
      | Type.String, s -> bprint_sql_quoted buf s in

    let do_cond0 i at =
      Attribute_type.soid at >|= fun at_id ->
      bprintf buf "q%d.attribute_type_id = %ld" i at_id in

    let do_cond1 i op at x =
      Attribute_type.soid at >|= fun at_id ->
      bprintf buf "q%d.attribute_type_id = %ld AND q%d.value %s " i at_id i op;
      do_value at x in

    let do_between i at x y =
      Attribute_type.soid at >|= fun at_id ->
      bprintf buf "q%d.attribute_type_id = %ld AND q%d.value >= " i at_id i;
      do_value at x;
      bprintf buf " AND q%d.value < " i;
      do_value at y in

    let do_in i at xs =
      Attribute_type.soid at >|= fun at_id ->
      assert (not (Values.is_empty xs));
      bprintf buf "q%d.attribute_type_id = %ld AND (" i at_id;
      let is_first = ref true in
      Values.iter
        (fun x ->
          if !is_first then is_first := false else Buffer.add_string buf " OR ";
          bprintf buf "q%d.value = " i;
          do_value at x)
        xs;
      Buffer.add_char buf ')' in

    let do_fts i x =
      bprintf buf "q%d.fts_vector @@ to_tsquery(q%d.fts_config::regconfig, "
              i i;
      bprint_sql_quoted buf x;
      bprintf buf ")";
      Lwt.return_unit in

    let do_cond i pred =
      Buffer.add_string buf (if i = 0 then " WHERE " else " AND ");
      match pred with
      | Relation.Inter _ -> assert false
      | Relation.Present at -> do_cond0 i at
      | Relation.Eq (at, x) -> do_cond1 i "=" at x
      | Relation.In (at, xs) -> do_in i at xs
      | Relation.Leq (at, x) -> do_cond1 i "<=" at x
      | Relation.Geq (at, x) -> do_cond1 i ">=" at x
      | Relation.Between (at, x, y) -> do_between i at x y
      | Relation.Search (at, x) -> do_cond1 i "SIMILAR TO" at x
      | Relation.Search_fts x -> do_fts i x in

    List.iteri do_join preds;
    Lwt_list.iteri_p do_cond preds

  let rec flatten = function
    | Relation.Inter ps -> List.flatten_map flatten ps
    | p -> [p]

  let select_image p ids =
    let buf = Buffer.create 512 in
    Buffer.add_string buf "SELECT q0.output_id FROM ";
    let%lwt () = bprint_adjacency_conj buf (flatten p) in
    begin match ids with
    | [] -> bprintf buf " AND false" (* FIXME *)
    | [id] -> bprintf buf " AND q0.input_id = %ld" id
    | id :: ids ->
      bprintf buf " AND (q0.input_id = %ld" id;
      List.iter (bprintf buf " OR q0.input_id = %ld") ids;
      Buffer.add_char buf ')'
    end;
    Lwt.return (oneshot_sql (Buffer.contents buf))

  let select_preimage p ids =
    let buf = Buffer.create 512 in
    Buffer.add_string buf "SELECT q0.input_id FROM ";
    let%lwt () = bprint_adjacency_conj buf (flatten p) in
    begin match ids with
    | [] -> bprintf buf " AND false" (* FIXME *)
    | [id] -> bprintf buf " AND q0.output_id = %ld" id
    | id :: ids ->
      bprintf buf " AND (q0.output_id = %ld" id;
      List.iter (bprintf buf " OR q0.output_id = %ld") ids;
      Buffer.add_char buf ')'
    end;
    Lwt.return (oneshot_sql (Buffer.contents buf))

end
