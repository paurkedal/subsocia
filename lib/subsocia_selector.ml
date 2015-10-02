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

open Printf
open Pwt_infix
open Subsocia_prereq
open Subsocia_common
open Unprime_string

include Subsocia_selector_types

let pred_char = '+'
let succ_char = '-'

let selector_of_string s =
  try Subsocia_lexer.selector_of_string s
  with Parsing.Parse_error -> raise (Invalid_argument "Parse error")

let is_reserved = function
  | '{' | '}' | '/' | '-' | '+' | ',' | '=' -> true
  | _ -> false

let p_slash = 0
let p_equal = 2
let p_disj = 3
let p_conj = 4

let bprint_attr dir op buf p k v =
  if p > p_equal then Buffer.add_char buf '{';
  Buffer.add_string buf k;
  Buffer.add_string buf op;
  if String.exists is_reserved v then begin
    Buffer.add_char buf '{';
    if dir = `Asuper then Buffer.add_char buf succ_char;
    Buffer.add_string buf v;
    Buffer.add_char buf '}'
  end else
    Buffer.add_string buf v;
  if p > p_equal then Buffer.add_char buf '}'

let rec bprint_selector buf p = function
  | Select_with (s0, s1) ->
    if p > p_slash then Buffer.add_char buf '{';
    bprint_selector buf p_slash s0;
    Buffer.add_char buf '/';
    bprint_selector buf p_slash s1;
    if p > p_slash then Buffer.add_char buf '}'
  | Select_adjacent (Asub (Attribute_eq ("unique_name", v)))
      when not (String.exists is_reserved v) ->
    Buffer.add_string buf v
  | Select_adjacent (Asub (Attribute_eq (k, v))) ->
    bprint_attr `Asub "=" buf p k v
  | Select_adjacent (Asuper (Attribute_eq (k, v))) ->
    bprint_attr `Asuper "=" buf p k v
  | Select_adjacent (Asub (Attribute_leq (k, v))) ->
    bprint_attr `Asub "<=" buf p k v
  | Select_adjacent (Asuper (Attribute_leq (k, v))) ->
    bprint_attr `Asuper "<=" buf p k v
  | Select_adjacent (Asub (Attribute_geq (k, v))) ->
    bprint_attr `Asub ">=" buf p k v
  | Select_adjacent (Asuper (Attribute_geq (k, v))) ->
    bprint_attr `Asuper ">=" buf p k v
  | Select_adjacent (Asub (Attribute_present k)) ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_string buf k;
    Buffer.add_string buf "=_";
    if p > p_equal then Buffer.add_char buf '}'
  | Select_adjacent (Asuper (Attribute_present k)) ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_char buf succ_char;
    Buffer.add_string buf k;
    Buffer.add_string buf "=_";
    if p > p_equal then Buffer.add_char buf '}'
  | Select_type tn ->
    Buffer.add_char buf ':';
    Buffer.add_string buf tn
  | Select_root -> Buffer.add_char buf '#'
  | Select_id id -> bprintf buf "#%ld" id
  | Select_adjacent Dsub ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_char buf pred_char;
    if p > p_equal then Buffer.add_char buf '}'
  | Select_adjacent Dsuper ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_char buf succ_char;
    if p > p_equal then Buffer.add_char buf '}'
  | Select_union (s0, s1) ->
    if p > p_disj then Buffer.add_char buf '{';
    bprint_selector buf p_disj s0;
    Buffer.add_char buf ',';
    bprint_selector buf p_disj s1;
    if p > p_disj then Buffer.add_char buf '}'
  | Select_inter (s0, s1) ->
    bprint_selector buf p_conj s0;
    bprint_selector buf p_conj s1

let string_of_selector s =
  let buf = Buffer.create 80 in
  bprint_selector buf p_slash s;
  Buffer.contents buf

module Selector_utils (C : Subsocia_intf.S) = struct

  let req_at an =
    match_lwt C.Attribute_type.of_name an with
    | None -> Lwt.fail (Failure ("No attribute type is named " ^ an))
    | Some at -> Lwt.return at

  let entype_ap = function
    | Attribute_present an ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Attribute.Present at
    | Attribute_eq (an, s) ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Attribute.Eq (at, Value.typed_of_string (C.Attribute_type.value_type at) s)
    | Attribute_leq (an, s) ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Attribute.Leq (at, Value.typed_of_string (C.Attribute_type.value_type at) s)
    | Attribute_geq (an, s) ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Attribute.Geq (at, Value.typed_of_string (C.Attribute_type.value_type at) s)

  let rec select_from = function
    | Select_with (selA, selB) -> fun es ->
      select_from selA es >>= select_from selB
    | Select_union (selA, selB) -> fun es ->
      lwt esA = select_from selA es in
      lwt esB = select_from selB es in
      Lwt.return (C.Entity.Set.union esA esB)
    | Select_inter (selA, selB) -> fun es ->
      lwt esA = select_from selA es in
      lwt esB = select_from selB es in
      Lwt.return (C.Entity.Set.inter esA esB)
    | Select_adjacent (Asub p) -> fun es ->
      lwt p = entype_ap p in
      C.Entity.Set.fold_s
	(fun e1 acc -> C.Entity.image1 p e1 >|= C.Entity.Set.union acc)
	es C.Entity.Set.empty
    | Select_adjacent (Asuper p) -> fun es ->
      lwt p = entype_ap p in
      C.Entity.Set.fold_s
	(fun e1 acc -> C.Entity.preimage1 p e1 >|= C.Entity.Set.union acc)
	es C.Entity.Set.empty
    | Select_type etn -> fun es ->
      lwt et = match_lwt C.Entity_type.of_name etn with
	       | Some et -> Lwt.return et
	       | None -> Lwt.fail (Failure ("No type named " ^ etn)) in
      C.Entity.Set.filter_s
	(fun e -> C.Entity.type_ e >|=
		  fun et' -> C.Entity_type.compare et et' = 0)
	es
    | Select_root -> fun es ->
      if C.Entity.Set.is_empty es then Lwt.return C.Entity.Set.empty else
      C.Entity.root >|= C.Entity.Set.singleton
    | Select_id id -> fun es ->
      if C.Entity.Set.is_empty es then Lwt.return C.Entity.Set.empty else
      C.Entity.of_id id >|= C.Entity.Set.singleton
    | Select_adjacent Dsub -> fun es ->
      C.Entity.Set.fold_s
	(fun e1 acc -> C.Entity.dsub e1 >|= C.Entity.Set.union acc)
	es C.Entity.Set.empty
    | Select_adjacent Dsuper -> fun es ->
      C.Entity.Set.fold_s
	(fun e1 acc -> C.Entity.dsuper e1 >|= C.Entity.Set.union acc)
	es C.Entity.Set.empty

  let select sel =
    lwt root = C.Entity.root in
    select_from sel (C.Entity.Set.singleton root)

  let select_one sel =
    lwt root = C.Entity.root in
    lwt es = select_from sel (C.Entity.Set.singleton root) in
    match C.Entity.Set.cardinal es with
    | 1 -> Lwt.return (C.Entity.Set.min_elt es)
    | 0 -> lwt_failure_f "No entity matches %s." (string_of_selector sel)
    | n -> lwt_failure_f "%d entities matches %s, need one."
			 n (string_of_selector sel)

  let select_opt sel =
    lwt root = C.Entity.root in
    lwt es = select_from sel (C.Entity.Set.singleton root) in
    match C.Entity.Set.cardinal es with
    | 0 -> Lwt.return_none
    | 1 -> Lwt.return (Some (C.Entity.Set.min_elt es))
    | n -> lwt_failure_f "%d entities matches %s, need one."
			 n (string_of_selector sel)
end
