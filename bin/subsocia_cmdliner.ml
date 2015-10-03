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

open Cmdliner
open Subsocia_common
open Subsocia_prereq
open Subsocia_selector
open Subsocia_selector_types

let value_type_parser s =
  try `Ok (Type.of_string s) with Invalid_argument msg -> `Error msg
let value_type_printer fmtr (Type.Ex vt) =
  Format.pp_print_string fmtr (Type.to_string vt)
let value_type_conv = value_type_parser, value_type_printer

let multiplicity_parser s =
  try `Ok (Multiplicity.of_string s)
  with Invalid_argument msg -> `Error msg
let multiplicity_printer fmtr mu =
  Format.pp_print_string fmtr (Multiplicity.to_string mu)
let multiplicity_conv = multiplicity_parser, multiplicity_printer

let selector_parser s =
  try `Ok (selector_of_string s)
  with Invalid_argument msg -> `Error msg
let selector_printer fmtr sel =
  Format.pp_print_string fmtr (string_of_selector sel)
let selector_conv = selector_parser, selector_printer

let aselector_parser ~with_presence s =
  let rec aux acc = function
    | Select_root | Select_id _ | Select_with _ | Select_union _
    | Select_dsub | Select_dsuper
    | Select_image (Attribute_leq _ | Attribute_geq _)
    | Select_preimage _
    | Select_type _
	as sel_att ->
      invalid_arg_f "The selector %s cannot be used for attribute assignement. \
		     It must be a conjunction of one or more attribute \
		     equalities." (string_of_selector sel_att)
    | Select_inter (selA, selB) -> aux (aux acc selB) selA
    | Select_image (Attribute_eq (an, av)) ->
      (an, Some av) :: acc
    | Select_image (Attribute_present an) ->
      if not with_presence then invalid_arg_f "Presence selector not allowed.";
      (an, None) :: acc in
  try
    `Ok
      begin match selector_of_string s with
      | Select_with (sel_ctx, sel_att) -> Some sel_ctx, aux [] sel_att
      | sel_att -> None, aux [] sel_att
      end
  with Invalid_argument msg -> `Error msg

let aselector_printer fmtr (ctx, asgn) =
  let select_attr = function
    | (an, None) -> Select_image (Attribute_present an)
    | (an, Some av) -> Select_image (Attribute_eq (an, av)) in
  let sel_attr =
    match asgn with
    | [] -> assert false
    | anv :: xs ->
      List.fold_left (fun acc anv -> Select_with (acc, select_attr anv))
		     (select_attr anv) xs in
  Format.pp_print_string fmtr @@
    string_of_selector
      (match ctx with None -> sel_attr
		    | Some sel_ctx -> Select_with (sel_ctx, sel_attr))

let aselector_conv =
  aselector_parser ~with_presence:false, aselector_printer
let aselector_pres_conv =
  aselector_parser ~with_presence:true, aselector_printer
