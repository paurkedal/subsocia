(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

module Arg = struct
  include Cmdliner.Arg

  let value_type =
    let parse s =
      try `Ok (Type.any_of_string s) with Invalid_argument msg -> `Error msg
    in
    let pp fmtr (Type.Any vt) = Format.pp_print_string fmtr (Type.to_string vt) in
    (parse, pp)

  let multiplicity =
    let parse s =
      try `Ok (Multiplicity.of_string s)
      with Invalid_argument msg -> `Error msg
    in
    let pp fmtr mu = Format.pp_print_string fmtr (Multiplicity.to_string mu) in
    (parse, pp)

  let selector =
    let parse s =
      try `Ok (selector_of_string s)
      with Invalid_argument msg -> `Error msg
    in
    let pp fmtr sel = Format.pp_print_string fmtr (string_of_selector sel) in
    (parse, pp)

  let add_selector =
    let parse s =
      try `Ok (add_selector_of_selector (selector_of_string s))
      with Invalid_argument msg -> `Error msg
    in
    let pp fmtr sel =
      Format.pp_print_string fmtr
        (string_of_selector (selector_of_add_selector sel))
    in
    (parse, pp)

  let delete_selector =
    let parse s =
      try `Ok (delete_selector_of_selector (selector_of_string s))
      with Invalid_argument msg -> `Error msg
    in
    let pp fmtr sel =
      Format.pp_print_string fmtr
        (string_of_selector (selector_of_delete_selector sel))
    in
    (parse, pp)
end
