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

open Printf
open Subsocia_prereq
open Subsocia_common
open Unprime_string

include Subsocia_selector_types

let selector_of_string s =
  try Subsocia_lexer.selector_of_string s
  with Parsing.Parse_error -> raise (Invalid_argument "Parse error")

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
  | Select_top -> Buffer.add_char buf '#'
  | Select_id id -> bprintf buf "#%ld" id
  | Select_pred ->
    if p > 1 then Buffer.add_char buf '{';
    Buffer.add_char buf '+';
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

  let rec select_from = function
    | Select_sub (selA, selB) -> fun es ->
      select_from selA es >>= select_from selB
    | Select_union (selA, selB) -> fun es ->
      lwt esA = select_from selA es in
      lwt esB = select_from selB es in
      Lwt.return (C.Entity.Set.union esA esB)
    | Select_inter (selA, selB) -> fun es ->
      lwt esA = select_from selA es in
      lwt esB = select_from selB es in
      Lwt.return (C.Entity.Set.inter esA esB)
    | Select_attr (an, v) -> fun es ->
      begin match_lwt C.Attribute_type.of_name an with
      | None -> Lwt.fail (Failure ("No attribute type is named " ^ an))
      | Some (C.Attribute_type.Ex at) ->
	let t = C.Attribute_type.type1 at in
	let x = Value.typed_of_string t v in
	C.Entity.Set.fold_s
	  (fun e1 acc -> C.Entity.apreds e1 at x >|= C.Entity.Set.union acc)
	  es C.Entity.Set.empty
      end
    | Select_top -> fun es ->
      if C.Entity.Set.is_empty es then Lwt.return C.Entity.Set.empty else
      C.Entity.top >|= C.Entity.Set.singleton
    | Select_id id -> fun es ->
      if C.Entity.Set.is_empty es then Lwt.return C.Entity.Set.empty else
      C.Entity.of_id id >|= C.Entity.Set.singleton
    | Select_pred -> fun es ->
      C.Entity.Set.fold_s
	(fun e1 acc -> C.Entity.preds e1 >|= C.Entity.Set.union acc)
	es C.Entity.Set.empty

  let select sel =
    lwt e_top = C.Entity.top in
    select_from sel (C.Entity.Set.singleton e_top)

  let select_one sel =
    lwt e_top = C.Entity.top in
    lwt es = select_from sel (C.Entity.Set.singleton e_top) in
    match C.Entity.Set.cardinal es with
    | 1 -> Lwt.return (C.Entity.Set.min_elt es)
    | 0 -> lwt_failure_f "No entity matches %s." (string_of_selector sel)
    | n -> lwt_failure_f "%d entities matches %s, need one."
			 n (string_of_selector sel)

  let select_opt sel =
    lwt e_top = C.Entity.top in
    lwt es = select_from sel (C.Entity.Set.singleton e_top) in
    match C.Entity.Set.cardinal es with
    | 0 -> Lwt.return_none
    | 1 -> Lwt.return (Some (C.Entity.Set.min_elt es))
    | n -> lwt_failure_f "%d entities matches %s, need one."
			 n (string_of_selector sel)
end
