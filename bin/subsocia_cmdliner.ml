(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Subsocia_common
open Subsocia_selector

let value_type_parser s =
  try `Ok (Type.any_of_string s) with Invalid_argument msg -> `Error msg
let value_type_printer fmtr (Type.Any vt) =
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

let add_selector_conv =
  let parse s =
    try `Ok (add_selector_of_selector (selector_of_string s))
    with Invalid_argument msg -> `Error msg in
  let print fmtr sel =
    Format.pp_print_string fmtr
      (string_of_selector (selector_of_add_selector sel)) in
  parse, print

let delete_selector_conv =
  let parse s =
    try `Ok (delete_selector_of_selector (selector_of_string s))
    with Invalid_argument msg -> `Error msg in
  let print fmtr sel =
    Format.pp_print_string fmtr
      (string_of_selector (selector_of_delete_selector sel)) in
  parse, print
