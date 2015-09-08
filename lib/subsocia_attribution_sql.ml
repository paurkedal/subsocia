(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
open Printf
open Subsocia_common
open Subsocia_intf

let table_for_type : type a. a Type.t -> string = function
  | Type.Bool -> "attribution_bool"
  | Type.Int -> "attribution_int"
  | Type.String -> "attribution_string"

let schema_prefix = ref "subsocia." (* FIXME: import *)

module type Arg = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Attribute : ATTRIBUTE with module Attribute_type := Attribute_type
end

module Make (Arg : Arg) = struct
  open Arg

  let table_for_predicate = function
    | Attribute.Present at -> table_for_type (Attribute_type.value_type at)
    | Attribute.Eq (at, _) -> table_for_type (Attribute_type.value_type at)
    | Attribute.Leq (at, _) -> table_for_type (Attribute_type.value_type at)
    | Attribute.Geq (at, _) -> table_for_type (Attribute_type.value_type at)
    | Attribute.Between(at,_,_) -> table_for_type (Attribute_type.value_type at)
    | Attribute.Search _ -> "attribution_string"
    | Attribute.Search_fts _ -> "attribution_string_fts"

  let bprint_predicate_conj buf preds =

    let do_join i pred =
      let tn = table_for_predicate pred in
      if i = 0 then
	bprintf buf "%s%s q%d" !schema_prefix tn i
      else
	bprintf buf " JOIN %s%s q%d \
			ON q%d.asub_id = q%d.asub_id \
		       AND q%d.asuper_id = q%d.asuper_id"
		!schema_prefix tn i i (i - 1) i (i - 1) in

    let do_value : type a. a Attribute_type.t -> a -> unit = fun at x ->
      match Attribute_type.value_type at, x with
      | Type.Bool, true -> Buffer.add_string buf "true"
      | Type.Bool, false -> Buffer.add_string buf "false"
      | Type.Int, i -> bprintf buf "%d" i
      | Type.String, s -> bprint_sql_quoted buf s in

    let do_cond0 i at =
      bprintf buf "q%d.attribute_type_id = %ld" i (Attribute_type.id' at) in

    let do_cond1 i op at x =
      bprintf buf "q%d.attribute_type_id = %ld AND q%d.value %s "
	      i (Attribute_type.id' at) i op;
      do_value at x in

    let do_between i at x y =
      bprintf buf "q%d.attribute_type_id = %ld AND q%d.value >= "
	      i (Attribute_type.id' at) i;
      do_value at x;
      bprintf buf " AND q%d.value < " i;
      do_value at y in

    let do_fts i x =
      bprintf buf "q%d.fts_vector @@ to_tsquery(q%d.fts_config::regconfig, "
	      i i;
      bprint_sql_quoted buf x;
      bprintf buf ")" in

    let do_cond i pred =
      Buffer.add_string buf (if i = 0 then " WHERE " else " AND ");
      match pred with
      | Attribute.Present at -> do_cond0 i at
      | Attribute.Eq (at, x) -> do_cond1 i "=" at x
      | Attribute.Leq (at, x) -> do_cond1 i "<=" at x
      | Attribute.Geq (at, x) -> do_cond1 i ">=" at x
      | Attribute.Between (at, x, y) -> do_between i at x y
      | Attribute.Search (at, x) -> do_cond1 i "SIMILAR TO" at x
      | Attribute.Search_fts x -> do_fts i x in

    List.iteri do_join preds;
    List.iteri do_cond preds

  let select_asub_conj id ps =
    let buf = Buffer.create 512 in
    Buffer.add_string buf "SELECT q0.asub_id FROM ";
    bprint_predicate_conj buf ps;
    bprintf buf " AND q0.asuper_id = %ld" id;
    oneshot_sql (Buffer.contents buf)

  let select_asuper_conj id ps =
    let buf = Buffer.create 512 in
    Buffer.add_string buf "SELECT q0.asuper_id FROM ";
    bprint_predicate_conj buf ps;
    bprintf buf " AND q0.asub_id = %ld" id;
    oneshot_sql (Buffer.contents buf)

end
