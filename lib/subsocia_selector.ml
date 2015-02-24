(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

include Subsocia_selector_types

let selector_of_string = Subsocia_selector_lexer.parse_string

let is_reserved = function
  | '{' | '}' | '/' | '+' | '=' -> true
  | _ -> false

let rec bprint_selector buf p = function
  | Select_sub (s0, s1) ->
    if p > 0 then Buffer.add_char buf '{';
    bprint_selector buf 0 s0;
    Buffer.add_char buf '/';
    bprint_selector buf 0 s1;
    if p > 0 then Buffer.add_char buf '}'
  | Select_attr (k, v) ->
    if p > 1 then Buffer.add_char buf '{';
    Buffer.add_string buf k;
    Buffer.add_char buf '=';
    if String.exists is_reserved v then begin
      Buffer.add_char buf '{';
      Buffer.add_string buf v;
      Buffer.add_char buf '}'
    end else
      Buffer.add_string buf v;
    if p > 1 then Buffer.add_char buf '}'
  | Select_union (s0, s1) ->
    if p > 2 then Buffer.add_char buf '{';
    bprint_selector buf 2 s0;
    Buffer.add_char buf '+';
    bprint_selector buf 2 s1;
    if p > 2 then Buffer.add_char buf '}'
  | Select_inter (s0, s1) ->
    bprint_selector buf 3 s0;
    bprint_selector buf 3 s1

let string_of_selector s =
  let buf = Buffer.create 80 in
  bprint_selector buf 0 s;
  Buffer.contents buf
