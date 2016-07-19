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

open Pwt_infix
open Subsocia_common
open Subsocia_prereq
open Subsocia_selector
open Subsocia_selector_types
open Unprime_list

module Log = struct
  (* Switched off to avoid dependency on lwt.unix. *)
  let warning _ = Lwt.return_unit
  let warning_f fmt = Printf.ksprintf warning fmt
end

let load = Subsocia_lexer.parse_schema

module Make (C : Subsocia_intf.S) = struct
  module Su = Selector_utils (C)

  let req_at atn =
    match%lwt C.Attribute_type.of_name atn with
    | Some at -> Lwt.return at
    | None -> lwt_failure_f "No attribute type is named %s." atn

  let req_et etn =
    match%lwt C.Entity_type.of_name etn with
    | Some et -> Lwt.return et
    | None -> lwt_failure_f "No entity type is named %s." etn

  let exec_et_adjust et = function
    | `Allow_inclusion (etn', mu, mu') ->
      let%lwt et' = req_et etn' in
      C.Entity_type.allow_dsub mu mu' et et'
    | `Disallow_inclusion etn' ->
      let%lwt et' = req_et etn' in
      C.Entity_type.disallow_dsub et et'
    | `Allow_attribution (etn', atn) ->
      let%lwt et' = req_et etn' in
      let%lwt C.Attribute_type.Ex at = req_at atn in
      C.Entity_type.allow_attribution at et' et
    | `Disallow_attribution (etn', atn) ->
      let%lwt et' = req_et etn' in
      let%lwt C.Attribute_type.Ex at = req_at atn in
      C.Entity_type.disallow_attribution at et' et
    | `Aux_string ("display", tmpl) ->
      C.Entity_type.set_entity_name_tmpl et tmpl
    | `Aux_string (p, _) ->
      lwt_failure_f "Entity types have no property %s." p

  let add_set_helper f e asel =
    let sel', attrs = add_selector_of_selector asel in
    let%lwt e' =
      match sel' with
      | None -> C.Entity.root
      | Some sel' -> Su.select_one sel' in
    String_map.iter_s
      (fun an vs ->
        match%lwt C.Attribute_type.of_name an with
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
    let sel', attrs = delete_selector_of_selector sel in
    let%lwt e' =
      match sel' with
      | None -> C.Entity.root
      | Some sel' -> Su.select_one sel' in
    String_map.iter_s
      (fun an vs_opt ->
        match%lwt C.Attribute_type.of_name an with
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
    | `Aux_selector (p, _) ->
      lwt_failure_f "Entities have no property %s." p
    | `Add_sub sel ->
      let%lwt e' = Su.select_one sel in
      C.Entity.force_dsub e e'
    | `Remove_sub sel ->
      let%lwt e' = Su.select_one sel in
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
      let%lwt ats = Lwt_list.map_s req_at atns in
      let ats = List.fold C.Attribute_type.Set.add ats
                          C.Attribute_type.Set.empty in
      begin match%lwt C.Attribute_uniqueness.find ats with
      | Some au ->
        Log.warning_f "Already constrained by #%ld."
                      (C.Attribute_uniqueness.id au)
      | None ->
        C.Attribute_uniqueness.force ats >|= ignore
      end
    | `Au_relax atns ->
      let%lwt ats = Lwt_list.map_s req_at atns in
      let ats = List.fold C.Attribute_type.Set.add ats
                          C.Attribute_type.Set.empty in
      begin match%lwt C.Attribute_uniqueness.find ats with
      | Some au -> C.Attribute_uniqueness.relax au
      | None -> Log.warning "Not constrained."
      end
    | `Et_create (etn, allows) ->
      let%lwt et = C.Entity_type.create etn in
      Lwt_list.iter_s (exec_et_adjust et) allows
    | `Et_modify (etn, adjusts) ->
      let%lwt et = req_et etn in
      Lwt_list.iter_s (exec_et_adjust et) adjusts
    | `Et_delete etn ->
      let%lwt et = C.Entity_type.of_name etn in
      Pwt_option.iter_s C.Entity_type.delete et
    | `Et_allow_dsub (etn0, etn1) ->
      let%lwt et0 = req_et etn0 in
      let%lwt et1 = req_et etn1 in
      C.Entity_type.allow_dsub Multiplicity.May Multiplicity.May et0 et1
    | `Et_disallow_dsub (etn0, etn1) ->
      let%lwt et0 = req_et etn0 in
      let%lwt et1 = req_et etn1 in
      C.Entity_type.disallow_dsub et0 et1
    | `Et_allow_attribution (atn, etn0, etn1) ->
      let%lwt C.Attribute_type.Ex at = req_at atn in
      let%lwt et0 = req_et etn0 in
      let%lwt et1 = req_et etn1 in
      C.Entity_type.allow_attribution at et0 et1
    | `Et_disallow_attribution (atn, etn0, etn1) ->
      let%lwt C.Attribute_type.Ex at = req_at atn in
      let%lwt et0 = req_et etn0 in
      let%lwt et1 = req_et etn1 in
      C.Entity_type.disallow_attribution at et0 et1
    | `Et_display (etn, template) ->
      let%lwt et = req_et etn in
      C.Entity_type.set_entity_name_tmpl et template
    | `E_force_dsub (sel0, sel1) ->
      let%lwt e0 = Su.select_one sel0 in
      let%lwt e1 = Su.select_one sel1 in
      C.Entity.force_dsub e0 e1
    | `E_relax_dsub (sel0, sel1) ->
      let%lwt e0 = Su.select_one sel0 in
      let%lwt e1 = Su.select_one sel1 in
      C.Entity.relax_dsub e0 e1
    | `E_add_value (atn, vr, sel0, sel1) ->
      let%lwt C.Attribute_type.Ex at = req_at atn in
      let vt = C.Attribute_type.value_type at in
      let v = Value.typed_of_string vt vr in
      let%lwt e0 = Su.select_one sel0 in
      let%lwt e1 = Su.select_one sel1 in
      let vs = Values.singleton (C.Attribute_type.value_type at) v in
      C.Entity.add_values at vs e0 e1
    | `E_remove_value (atn, vr, sel0, sel1) ->
      let%lwt C.Attribute_type.Ex at = req_at atn in
      let vt = C.Attribute_type.value_type at in
      let v = Value.typed_of_string vt vr in
      let%lwt e0 = Su.select_one sel0 in
      let%lwt e1 = Su.select_one sel1 in
      let vs = Values.singleton (C.Attribute_type.value_type at) v in
      C.Entity.remove_values at vs e0 e1
    | `E_create (sel, etn) ->
      let%lwt et = req_et etn in
      let%lwt e = C.Entity.create et in
      (* TODO: Make sure sel is a path. *)
      add_set_helper `Add e sel
    | `Create (etn, addl) ->
      begin match%lwt C.Entity_type.of_name etn with
      | Some et ->
        let%lwt e = C.Entity.create et in
        Lwt_list.iter_s (exec_mod e) addl
      | None ->
        lwt_failure_f "No entity type is called %s." etn
      end
    | `Modify (sel, modl) ->
      let%lwt e = Su.select_one sel in
      Lwt_list.iter_s (exec_mod e) modl
    | `Delete sel ->
      let%lwt e = Su.select_one sel in
      C.Entity.delete e

  let exec = Lwt_list.iter_s exec_schema_entry
end
