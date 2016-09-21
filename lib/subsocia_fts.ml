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

open Unprime_string

type t = Subsocia_internal.fts

let tsquery s = s

let is_nonword_char = function
 | '0'..'9' | 'A'..'Z' | 'a'..'z' | '\x80'..'\xff' -> false
 | _ -> true

let of_completion_string s =
  match String.chop_consecutive is_nonword_char s |> List.filter ((<>) "") with
   | [] -> None
   | words -> Some (String.concat " & " words ^ ":*")
