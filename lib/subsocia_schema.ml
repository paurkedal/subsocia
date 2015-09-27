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

open Pwt_infix
open Subsocia_common
open Subsocia_prereq
open Subsocia_selector
open Unprime_list

let load = Subsocia_lexer.parse_schema

let rec aconj_of_selector = function
  | Select_with _ | Select_union _ | Select_top | Select_id _
  | Select_adjacent (Dsub | Dsuper
		      | Asub (Attribute_present _ | Attribute_leq _
						  | Attribute_geq _) | Asuper _)
  | Select_type _
      as sel_att -> fun _ ->
    invalid_arg_f "The selector %s cannot be used for attribute assignement. \
		   It must be a conjunction of one or more attribute \
		   equalities." (string_of_selector sel_att)
  | Select_inter (selA, selB) -> fun m ->
    aconj_of_selector selA (aconj_of_selector selB m)
  | Select_adjacent (Asub (Attribute_eq (an, v))) -> fun m ->
    let vs = try String_map.find an m with Not_found -> [] in
    String_map.add an (v :: vs) m

let aselector_of_selector = function
  | Select_with (sel_ctx, sel_att) ->
    Some sel_ctx, aconj_of_selector sel_att String_map.empty
  | sel_att ->
    None, aconj_of_selector sel_att String_map.empty

let rec dconj_of_selector = function
  | Select_with _ | Select_union _ | Select_top | Select_id _
  | Select_adjacent (Dsub | Dsuper |
		     Asuper _ | Asub (Attribute_leq _ | Attribute_geq _))
  | Select_type _
      as sel_att -> fun _ ->
    invalid_arg_f "The selector %s cannot be used for attribute assignement. \
		   It must be a conjunction of one or more attribute \
		   equalities." (string_of_selector sel_att)
  | Select_inter (selA, selB) -> fun m ->
    dconj_of_selector selA (dconj_of_selector selB m)
  | Select_adjacent (Asub (Attribute_present an)) -> fun m ->
    if String_map.contains an m then
      invalid_arg_f "Conflicting wildcard for %s." an;
    String_map.add an None m
  | Select_adjacent (Asub (Attribute_eq (an, v))) -> fun m ->
    let vs =
      try
	match String_map.find an m with
	| None -> invalid_arg_f "Conflicting wildcard for %s." an;
	| Some vs -> vs
      with Not_found -> [] in
    String_map.add an (Some (v :: vs)) m

let dselector_of_selector = function
  | Select_with (sel_ctx, sel_att) ->
    Some sel_ctx, dconj_of_selector sel_att String_map.empty
  | sel_att ->
    None, dconj_of_selector sel_att String_map.empty

module Make (C : Subsocia_intf.S) = struct
  module Su = Selector_utils (C)

  let req_at atn =
    match_lwt C.Attribute_type.of_name atn with
    | Some at -> Lwt.return at
    | None -> lwt_failure_f "No attribute type is named %s." atn

  let req_et etn =
    match_lwt C.Entity_type.of_name etn with
    | Some et -> Lwt.return et
    | None -> lwt_failure_f "No entity type is named %s." etn

  let exec_et_adjust et = function
    | `Allow_inclusion (etn', mu, mu') ->
      lwt et' = req_et etn' in
      C.Entity_type.allow_dsub mu mu' et et'
    | `Disallow_inclusion etn' ->
      lwt et' = req_et etn' in
      C.Entity_type.disallow_dsub et et'
    | `Allow_attribution (etn', atn) ->
      lwt et' = req_et etn' in
      lwt C.Attribute_type.Ex at = req_at atn in
      C.Entity_type.allow_attribution at et' et
    | `Disallow_attribution (etn', atn) ->
      lwt et' = req_et etn' in
      lwt C.Attribute_type.Ex at = req_at atn in
      C.Entity_type.disallow_attribution at et' et
    | `Aux_string ("display", tmpl) ->
      C.Entity_type.set_entity_name_tmpl et tmpl
    | `Aux_string (p, _) ->
      lwt_failure_f "Entity types have no property %s." p

  let add_set_helper f e asel =
    let sel', attrs = aselector_of_selector asel in
    lwt e' =
      match sel' with
      | None -> C.Entity.top
      | Some sel' -> Su.select_one sel' in
    String_map.iter_s
      (fun an vs ->
	match_lwt C.Attribute_type.of_name an with
	| None -> lwt_failure_f "No attribute type is named %s." an
	| Some (C.Attribute_type.Ex at) ->
	  let t = C.Attribute_type.value_type at in
	  let f = match f with `Add -> C.Entity.add_values
			     | `Set -> C.Entity.set_values in
	  let vs = List.map (Value.typed_of_string t) vs in
	  let vs = Values.of_elements t vs in
	  f at vs e' e)
      attrs

  let del_helper e sel =
    let sel', attrs = dselector_of_selector sel in
    lwt e' =
      match sel' with
      | None -> C.Entity.top
      | Some sel' -> Su.select_one sel' in
    String_map.iter_s
      (fun an vs_opt ->
	match_lwt C.Attribute_type.of_name an with
	| None -> lwt_failure_f "No attribute type is named %s." an
	| Some (C.Attribute_type.Ex at) ->
	  match vs_opt with
	  | None ->
	    let vs = Values.empty (C.Attribute_type.value_type at) in
	    C.Entity.set_values at vs e' e
	  | Some vs ->
	    let t = C.Attribute_type.value_type at in
	    let vs = List.map (Value.typed_of_string t) vs in
	    let vs = Values.of_elements t vs in
	    C.Entity.remove_values at vs e' e)
      attrs

  let exec_mod e = function
    | `Aux_selector ("access", sel) ->
      lwt access = Su.select_one sel in
      C.Entity.modify ~access e
    | `Aux_selector (p, _) ->
      lwt_failure_f "Entities have no property %s." p
    | `Add_sub sel ->
      lwt e' = Su.select_one sel in
      C.Entity.force_dsub e e'
    | `Remove_sub sel ->
      lwt e' = Su.select_one sel in
      C.Entity.relax_dsub e e'
    | `Add_attr asel -> add_set_helper `Add e asel
    | `Set_attr asel -> add_set_helper `Set e asel
    | `Remove_attr sel' -> del_helper e sel'

  let exec_schema_entry = function
    | `At_create (atn, tn) ->
      let Type.Ex t = Type.of_string tn in
      C.Attribute_type.create t atn >|= fun _ -> ()
    | `At_delete atn ->
      C.Attribute_type.of_name atn >>=
      Pwt_option.iter_s
	(fun (C.Attribute_type.Ex at) -> C.Attribute_type.delete at)
    | `Au_force atns ->
      lwt ats = Lwt_list.map_s req_at atns in
      let ats = List.fold C.Attribute_type.Set.add ats
			  C.Attribute_type.Set.empty in
      begin match_lwt C.Attribute_uniqueness.find ats with
      | Some au ->
	Lwt_log.warning_f "Already constrained by #%ld."
			  (C.Attribute_uniqueness.id au)
      | None ->
	C.Attribute_uniqueness.force ats >|= ignore
      end
    | `Et_create (etn, allows) ->
      lwt et = C.Entity_type.create etn in
      Lwt_list.iter_s (exec_et_adjust et) allows
    | `Et_modify (etn, adjusts) ->
      begin match_lwt C.Entity_type.of_name etn with
      | Some et ->
	Lwt_list.iter_s (exec_et_adjust et) adjusts
      | None ->
	lwt_failure_f "No entity type is called %s." etn
      end
    | `Et_delete etn ->
      lwt et = C.Entity_type.of_name etn in
      Pwt_option.iter_s C.Entity_type.delete et
    | `Create (etn, addl) ->
      begin match_lwt C.Entity_type.of_name etn with
      | Some et ->
	lwt e = C.Entity.create et in
	Lwt_list.iter_s (exec_mod e) addl
      | None ->
	lwt_failure_f "No entity type is called %s." etn
      end
    | `Modify (sel, modl) ->
      lwt e = Su.select_one sel in
      Lwt_list.iter_s (exec_mod e) modl
    | `Delete sel ->
      lwt e = Su.select_one sel in
      C.Entity.delete e

  let exec = Lwt_list.iter_s exec_schema_entry
end
