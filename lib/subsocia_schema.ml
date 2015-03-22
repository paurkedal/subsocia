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
open Subsocia_prereq
open Subsocia_selector

let load_schema = Subsocia_lexer.parse_schema

let rec aconj_of_selector = function
  | Select_sub _ | Select_union _ | Select_pred | Select_top | Select_id _
  | Select_attr_present _ as sel_att -> fun _ ->
    invalid_arg_f "The selector %s cannot be used for attribute assignement. \
		   It must be a conjunction of one or more attribute \
		   equalities." (string_of_selector sel_att)
  | Select_inter (selA, selB) -> fun m ->
    aconj_of_selector selA (aconj_of_selector selB m)
  | Select_attr (an, v) -> fun m ->
    let vs = try String_map.find an m with Not_found -> [] in
    String_map.add an (v :: vs) m

let aselector_of_selector = function
  | Select_sub (sel_ctx, sel_att) ->
    Some sel_ctx, aconj_of_selector sel_att String_map.empty
  | sel_att ->
    None, aconj_of_selector sel_att String_map.empty

let rec dconj_of_selector = function
  | Select_sub _ | Select_union _ | Select_pred | Select_top | Select_id _
      as sel_att -> fun _ ->
    invalid_arg_f "The selector %s cannot be used for attribute assignement. \
		   It must be a conjunction of one or more attribute \
		   equalities." (string_of_selector sel_att)
  | Select_inter (selA, selB) -> fun m ->
    dconj_of_selector selA (dconj_of_selector selB m)
  | Select_attr_present an -> fun m ->
    if String_map.contains an m then
      invalid_arg_f "Conflicting wildcard for %s." an;
    String_map.add an None m
  | Select_attr (an, v) -> fun m ->
    let vs =
      try
	match String_map.find an m with
	| None -> invalid_arg_f "Conflicting wildcard for %s." an;
	| Some vs -> vs
      with Not_found -> [] in
    String_map.add an (Some (v :: vs)) m

let dselector_of_selector = function
  | Select_sub (sel_ctx, sel_att) ->
    Some sel_ctx, dconj_of_selector sel_att String_map.empty
  | sel_att ->
    None, dconj_of_selector sel_att String_map.empty

let exec_schema (module C : Subsocia_intf.S) =
  let module C = Subsocia_derived.Make (C) in

  let add_set_helper f e asel =
    let sel', attrs = aselector_of_selector asel in
    lwt e' =
      match sel' with
      | None -> C.Entity.top
      | Some sel' -> C.Entity.select_one sel' in
    String_map.iter_s
      (fun an vs ->
	match_lwt C.Attribute_type.of_name an with
	| None -> lwt_failure_f "No attribute type is named %s." an
	| Some (C.Attribute_type.Ex at) ->
	  let t = C.Attribute_type.type1 at in
	  let f = match f with `Add -> C.Entity.addattr
			     | `Set -> C.Entity.setattr in
	  f e e' at (List.map (Value.typed_of_string t) vs))
      attrs in

  let del_helper e sel =
    let sel', attrs = dselector_of_selector sel in
    lwt e' =
      match sel' with
      | None -> C.Entity.top
      | Some sel' -> C.Entity.select_one sel' in
    String_map.iter_s
      (fun an vs_opt ->
	match_lwt C.Attribute_type.of_name an with
	| None -> lwt_failure_f "No attribute type is named %s." an
	| Some (C.Attribute_type.Ex at) ->
	  match vs_opt with
	  | None -> C.Entity.setattr e e' at []
	  | Some vs ->
	    let t = C.Attribute_type.type1 at in
	    C.Entity.delattr e e' at (List.map (Value.typed_of_string t) vs))
      attrs in

  let exec_mod e = function
    | `Add_sub sel ->
      lwt e' = C.Entity.select_one sel in
      C.Entity.constrain e e'
    | `Remove_sub sel ->
      lwt e' = C.Entity.select_one sel in
      C.Entity.unconstrain e e'
    | `Add_attr asel -> add_set_helper `Add e asel
    | `Set_attr asel -> add_set_helper `Set e asel
    | `Remove_attr sel' -> del_helper e sel' in

  let exec_schema_entry = function
    | `Create (etn, addl) ->
      begin match_lwt C.Entity_type.of_name etn with
      | Some et ->
	lwt viewer = C.Const.e_default_viewers in
	lwt admin = C.Const.e_default_admins in
	lwt e = C.Entity.create ~viewer ~admin et in
	Lwt_list.iter_s (exec_mod e) addl
      | None ->
	lwt_failure_f "No entity type is called %s." etn
      end
    | `Modify (sel, modl) ->
      lwt e = C.Entity.select_one sel in
      Lwt_list.iter_s (exec_mod e) modl
    | `Delete sel ->
      lwt e = C.Entity.select_one sel in
      C.Entity.delete e in
  Lwt_list.iter_s exec_schema_entry
