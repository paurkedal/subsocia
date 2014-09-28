(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

module Multiplicity = struct
  type t =
    | May1
    | Must1
    | May
    | Must

  let ( * ) a b =
    match a, b with
    | Must1,   _ |    _, Must1 -> b
    | May,     _ |    _, May   -> May
    | May1, May1               -> May1
    | May1, Must | Must, May1  -> May
    | Must, Must               -> Must

  let of_int = function
    | 0 -> May1
    | 1 -> Must1
    | 2 -> May
    | 3 -> Must
    | _ -> invalid_arg "Multiplicity.of_int"

  let to_int = function
    | May1 -> 0
    | Must1 -> 1
    | May -> 2
    | Must -> 3

  let of_char = function
    | '?' -> May1
    | '1' -> Must1
    | '*' -> May
    | '+' -> Must
    | _ -> invalid_arg "Multiplicity.of_char"

  let to_char = function
    | May1 -> '?'
    | Must1 -> '1'
    | May -> '*'
    | Must -> '+'

  let of_string s =
    if String.length s = 1 then of_char s.[0] else
    invalid_arg "Multiplicity.of_string"

  let to_string m = String.make 1 (to_char m)
end
