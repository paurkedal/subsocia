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

open Printf
open Pwt_infix
open Subsocia_common
open Subsocia_prereq
open Subsocia_selector_types
open Unprime_list
open Unprime_string

let pred_char = '+'
let succ_char = '-'

let selector_of_string s =
  try Subsocia_lexer.selector_of_string s
  with Parsing.Parse_error -> raise (Invalid_argument "Parse error")

let must_escape_char ~edge = function
  | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\x80'..'\xff' -> false
  | ' ' | '@' | '-' -> edge
  | _ -> true

let must_escape s =
  let n = String.length s in
  if n = 0 then true else
  if n = 1 && s.[0] = '_' then true else
  let rec loop i =
    if i = n then false else
    must_escape_char ~edge:(i = 0 || i = n - 1) s.[i] || loop (i + 1) in
  loop 0

let p_slash = 0
let p_equal = 2
let p_disj = 3
let p_conj = 4

let bprint_attr dir op buf p k v =
  if p > p_equal then Buffer.add_char buf '{';
  Buffer.add_string buf k;
  Buffer.add_string buf op;
  if must_escape v then begin
    Buffer.add_char buf '{';
    if dir = `Asuper then Buffer.add_char buf succ_char;
    Buffer.add_string buf v;
    Buffer.add_char buf '}'
  end else
    Buffer.add_string buf v;
  if p > p_equal then Buffer.add_char buf '}'

let rec bprint_selector ?(is_prefix = false) buf p = function
  | Select_with (s0, s1) ->
    if p > p_slash then Buffer.add_char buf '{';
    bprint_selector ~is_prefix:true buf p_slash s0;
    Buffer.add_char buf '/';
    bprint_selector buf p_slash s1;
    if p > p_slash then Buffer.add_char buf '}'
  | Select_image (Attribute_eq ("unique_name", v))
      when not (must_escape v) ->
    Buffer.add_string buf v
  | Select_image (Attribute_eq (k, v)) ->
    bprint_attr `Asub "=" buf p k v
  | Select_preimage (Attribute_eq (k, v)) ->
    bprint_attr `Asuper "=" buf p k v
  | Select_image (Attribute_leq (k, v)) ->
    bprint_attr `Asub "<=" buf p k v
  | Select_preimage (Attribute_leq (k, v)) ->
    bprint_attr `Asuper "<=" buf p k v
  | Select_image (Attribute_geq (k, v)) ->
    bprint_attr `Asub ">=" buf p k v
  | Select_preimage (Attribute_geq (k, v)) ->
    bprint_attr `Asuper ">=" buf p k v
  | Select_image (Attribute_present k) ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_string buf k;
    Buffer.add_string buf "=_";
    if p > p_equal then Buffer.add_char buf '}'
  | Select_preimage (Attribute_present k) ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_char buf succ_char;
    Buffer.add_string buf k;
    Buffer.add_string buf "=_";
    if p > p_equal then Buffer.add_char buf '}'
  | Select_type tn ->
    Buffer.add_char buf ':';
    Buffer.add_string buf tn
  | Select_root -> if not is_prefix then Buffer.add_char buf '#'
  | Select_id id [@ocaml.warning "-3"] -> bprintf buf "#%ld" id
  | Select_dsub ->
    if p > p_equal then Buffer.add_char buf '{';
    Buffer.add_char buf pred_char;
    if p > p_equal then Buffer.add_char buf '}'
  | Select_dsuper ->
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
  if s = Select_root then "/" else
  let buf = Buffer.create 80 in
  bprint_selector ~is_prefix:true buf p_slash s;
  Buffer.contents buf

let rec longest_prefix = function
  | Select_with (pfx, sel) ->
    begin match longest_prefix sel with
    | None, subsel -> Some pfx, subsel
    | Some pfx', subsel -> Some (Select_with (pfx, pfx')), subsel
    end
  | sel -> None, sel

let rec aconj_of_selector = function
  | Select_with _ | Select_union _ | Select_root
  | Select_id _ [@ocaml.warning "-3"]
  | Select_dsub | Select_dsuper
  | Select_image (Attribute_present _ | Attribute_leq _ | Attribute_geq _)
  | Select_preimage _
  | Select_type _
      as sel_att -> fun _ ->
    invalid_arg_f "The selector %s cannot be used for attribute assignement. \
                   It must be a conjunction of one or more attribute \
                   equalities." (string_of_selector sel_att)
  | Select_inter (selA, selB) -> fun m ->
    aconj_of_selector selA (aconj_of_selector selB m)
  | Select_image (Attribute_eq (an, v)) -> fun m ->
    let vs = try String_map.find an m with Not_found -> [] in
    String_map.add an (v :: vs) m

let add_selector_of_selector sel =
  let pfx, subsel = longest_prefix sel in
  pfx, aconj_of_selector subsel String_map.empty

let rec dconj_of_selector = function
  | Select_with _ | Select_union _ | Select_root
  | Select_id _ [@ocaml.warning "-3"]
  | Select_dsub | Select_dsuper
  | Select_image (Attribute_leq _ | Attribute_geq _)
  | Select_preimage _
  | Select_type _
      as sel_att -> fun _ ->
    invalid_arg_f "The selector %s cannot be used for attribute deletion. \
                   It must be a conjunction of one or more attribute \
                   equalities." (string_of_selector sel_att)
  | Select_inter (selA, selB) -> fun m ->
    dconj_of_selector selA (dconj_of_selector selB m)
  | Select_image (Attribute_present an) -> fun m ->
    if String_map.contains an m then
      invalid_arg_f "Conflicting wildcard for %s." an;
    String_map.add an None m
  | Select_image (Attribute_eq (an, v)) -> fun m ->
    let vs =
      try
        match String_map.find an m with
        | None -> invalid_arg_f "Conflicting wildcard for %s." an;
        | Some vs -> vs
      with Not_found -> [] in
    String_map.add an (Some (v :: vs)) m

let delete_selector_of_selector sel =
  let pfx, subsel = longest_prefix sel in
  pfx, dconj_of_selector subsel String_map.empty

let selector_of_add_selector (ctx, assignments) =
  let sel = String_map.bindings assignments
    |> List.flatten_map
        (fun (an, avs) ->
          List.map (fun av -> Select_image (Attribute_eq (an, av))) avs)
    |> function
        | [] -> invalid_arg "selector_of_add_selector"
        | sel :: sels ->
          List.fold_right (fun a b -> Select_inter (a, b)) sels sel in
  match ctx with
  | None -> sel
  | Some ctx -> Select_with (ctx, sel)

let selector_of_delete_selector (ctx, assignments) =
  let sel = String_map.bindings assignments
    |> List.flatten_map
        (function
          | (an, Some avs) ->
            List.map (fun av -> Select_image (Attribute_eq (an, av))) avs
          | (an, None) ->
            [Select_image (Attribute_present an)])
    |> function
        | [] -> invalid_arg "selector_of_delete_selector"
        | sel :: sels ->
          List.fold_right (fun a b -> Select_inter (a, b)) sels sel in
  match ctx with
  | None -> sel
  | Some ctx -> Select_with (ctx, sel)

module Selector_utils (C : Subsocia_intf.S) = struct

  let req_at an =
    match%lwt C.Attribute_type.of_name an with
    | None -> Lwt.fail (Failure ("No attribute type is named " ^ an))
    | Some at -> Lwt.return at

  let entype_ap = function
    | Attribute_present an ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Relation.Present at
    | Attribute_eq (an, s) ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Relation.Eq (at, Value.typed_of_string (C.Attribute_type.value_type at) s)
    | Attribute_leq (an, s) ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Relation.Leq (at, Value.typed_of_string (C.Attribute_type.value_type at) s)
    | Attribute_geq (an, s) ->
      req_at an >|= fun (C.Attribute_type.Ex at) ->
      C.Relation.Geq (at, Value.typed_of_string (C.Attribute_type.value_type at) s)

  let rec select_from = function
    | Select_with (selA, selB) -> fun es ->
      select_from selA es >>= select_from selB
    | Select_union (selA, selB) -> fun es ->
      let%lwt esA = select_from selA es in
      let%lwt esB = select_from selB es in
      Lwt.return (C.Entity.Set.union esA esB)
    | Select_inter (selA, selB) -> fun es ->
      let%lwt esA = select_from selA es in
      let%lwt esB = select_from selB es in
      Lwt.return (C.Entity.Set.inter esA esB)
    | Select_image p -> fun es ->
      let%lwt p = entype_ap p in
      C.Entity.Set.fold_s
        (fun e1 acc -> C.Entity.image1 p e1 >|= C.Entity.Set.union acc)
        es C.Entity.Set.empty
    | Select_preimage p -> fun es ->
      let%lwt p = entype_ap p in
      C.Entity.Set.fold_s
        (fun e1 acc -> C.Entity.preimage1 p e1 >|= C.Entity.Set.union acc)
        es C.Entity.Set.empty
    | Select_type etn -> fun es ->
      let%lwt et =
        match%lwt C.Entity_type.of_name etn with
        | Some et -> Lwt.return et
        | None -> Lwt.fail (Failure ("No type named " ^ etn)) in
      C.Entity.Set.filter_s
        (fun e -> C.Entity.entity_type e >|=
                  fun et' -> C.Entity_type.compare et et' = 0)
        es
    | Select_root -> fun es ->
      if C.Entity.Set.is_empty es then Lwt.return C.Entity.Set.empty else
      C.Entity.root >|= C.Entity.Set.singleton
    | Select_id id [@ocaml.warning "-3"] -> fun es ->
      let soid =
        C.Entity.Soid.of_string (sprintf Subsocia_internal.e_soid_format id) in
      if C.Entity.Set.is_empty es then Lwt.return C.Entity.Set.empty else
      C.Entity.of_soid soid >|= C.Entity.Set.singleton
    | Select_dsub -> fun es ->
      C.Entity.Set.fold_s
        (fun e1 acc -> C.Entity.dsub e1 >|= C.Entity.Set.union acc)
        es C.Entity.Set.empty
    | Select_dsuper -> fun es ->
      C.Entity.Set.fold_s
        (fun e1 acc -> C.Entity.dsuper e1 >|= C.Entity.Set.union acc)
        es C.Entity.Set.empty

  let select sel =
    let%lwt root = C.Entity.root in
    select_from sel (C.Entity.Set.singleton root)

  let select_one sel =
    let%lwt root = C.Entity.root in
    let%lwt es = select_from sel (C.Entity.Set.singleton root) in
    match C.Entity.Set.cardinal es with
    | 1 -> Lwt.return (C.Entity.Set.min_elt es)
    | 0 -> lwt_failure_f "No entity matches %s." (string_of_selector sel)
    | n -> lwt_failure_f "%d entities matches %s, need one."
                         n (string_of_selector sel)

  let select_opt sel =
    let%lwt root = C.Entity.root in
    let%lwt es = select_from sel (C.Entity.Set.singleton root) in
    match C.Entity.Set.cardinal es with
    | 0 -> Lwt.return_none
    | 1 -> Lwt.return (Some (C.Entity.Set.min_elt es))
    | n -> lwt_failure_f "%d entities matches %s, need one."
                         n (string_of_selector sel)
end
