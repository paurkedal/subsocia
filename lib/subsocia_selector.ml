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

open Subsocia_common
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

module Selector_utils (C : Subsocia_intf.S) = struct
  let rec denote_selector = function
    | Select_sub (selA, selB) -> fun es ->
      denote_selector selA es >>= denote_selector selB
    | Select_union (selA, selB) -> fun es ->
      lwt esA = denote_selector selA es in
      lwt esB = denote_selector selB es in
      Lwt.return (C.Entity.Set.union esA esB)
    | Select_inter (selA, selB) -> fun es ->
      lwt esA = denote_selector selA es in
      lwt esB = denote_selector selB es in
      Lwt.return (C.Entity.Set.inter esA esB)
    | Select_attr (k, v) -> fun es ->
      match_lwt C.Attribute_type.of_name k with
      | None -> Lwt.fail (Failure ("Invalid attribute type " ^ k))
      | Some (C.Attribute_type.Ex at) ->
	let t = C.Attribute_type.type1 at in
	let x = Value.typed_of_string t v in
	C.Entity.Set.fold_s
	  (fun e1 acc -> C.Entity.apreds e1 at x >|= C.Entity.Set.union acc)
	  es C.Entity.Set.empty
end
