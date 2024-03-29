(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Lwt.Infix
open Lwt.Syntax
open Subsocia_common
open Subsocia_prereq
open Subsocia_selector
open Unprime_list

module Log = struct
  (* Switched off to avoid dependency on lwt.unix. *)
  let warning _ = Lwt.return_unit
  let warning_f fmt = Printf.ksprintf warning fmt
end

let load = Subsocia_lexer.parse_schema

module Make (C : Subsocia_intf.S) = struct
  module Su = Selector_utils (C)

  let req_et etn =
    C.Entity_type.of_name etn >>= function
     | Some et -> Lwt.return et
     | None -> Subsocia_error.fail_lwt "No entity type is named %s." etn

  let exec_et_adjust et = function
    | `Allow_inclusion (etn', mu, mu') ->
      let* et' = req_et etn' in
      C.Entity_type.allow_dsub mu mu' et et'
    | `Disallow_inclusion etn' ->
      let* et' = req_et etn' in
      C.Entity_type.disallow_dsub et et'
    | `Allow_attribution (etn', atn) ->
      let* et' = req_et etn' in
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      C.Entity_type.allow_attribution at et' et
    | `Disallow_attribution (etn', atn) ->
      let* et' = req_et etn' in
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      C.Entity_type.disallow_attribution at et' et
    | `Aux_string ("display", tmpl) ->
      C.Entity_type.set_entity_name_tmpl et tmpl
    | `Aux_string (p, _) ->
      Subsocia_error.fail_lwt "Entity types have no property %s." p

  let add_set_helper f e asel =
    let sel', attrs = add_selector_of_selector asel in
    let* e' =
      match sel' with
      | None -> C.Entity.get_root ()
      | Some sel' -> Su.select_one sel' in
    String_map.iter_s
      (fun an vs ->
        let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn an in
        let t = C.Attribute_type.value_type at in
        let f = match f with `Add -> C.Entity.add_values
                           | `Set -> C.Entity.set_values in
        let vs = List.map (Value.typed_of_string t) vs in
        let vs = Values.of_elements t vs in
        f at vs e' e)
      attrs

  let del_helper e sel =
    let sel', attrs = delete_selector_of_selector sel in
    let* e' =
      match sel' with
      | None -> C.Entity.get_root ()
      | Some sel' -> Su.select_one sel' in
    String_map.iter_s
      (fun an vs_opt ->
        let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn an in
        (match vs_opt with
         | None ->
            let vs = Values.empty (C.Attribute_type.value_type at) in
            C.Entity.set_values at vs e' e
         | Some vs ->
            let t = C.Attribute_type.value_type at in
            let vs = List.map (Value.typed_of_string t) vs in
            let vs = Values.of_elements t vs in
            C.Entity.remove_values at vs e' e))
      attrs

  let exec_mod e = function
    | `Aux_selector (p, _) ->
      Subsocia_error.fail_lwt "Entities have no property %s." p
    | `Add_sub sel ->
      let* e' = Su.select_one sel in
      C.Entity.force_dsub e e'
    | `Remove_sub sel ->
      let* e' = Su.select_one sel in
      C.Entity.relax_dsub e e'
    | `Add_attr asel -> add_set_helper `Add e asel
    | `Set_attr asel -> add_set_helper `Set e asel
    | `Remove_attr sel' -> del_helper e sel'

  let exec_schema_entry = function
    | `At_create (atn, tn) ->
      let Type.Any t = Type.any_of_string tn in
      C.Attribute_type.create t atn >|= fun _ -> ()
    | `At_delete atn ->
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      C.Attribute_type.delete at
    | `Au_force atns ->
      let* ats = Lwt_list.map_s C.Attribute_type.any_of_name_exn atns in
      let ats = List.fold C.Attribute_type.Set.add ats
                          C.Attribute_type.Set.empty in
      (C.Attribute_uniqueness.find ats >>= function
       | Some au ->
          let* au_id = C.Attribute_uniqueness.soid au in
          Log.warning_f "Already constrained by %s."
                        (C.Attribute_uniqueness.Soid.to_string au_id)
       | None ->
          C.Attribute_uniqueness.force ats >|= ignore)
    | `Au_relax atns ->
      let* ats = Lwt_list.map_s C.Attribute_type.any_of_name_exn atns in
      let ats = List.fold C.Attribute_type.Set.add ats
                          C.Attribute_type.Set.empty in
      (C.Attribute_uniqueness.find ats >>= function
       | Some au -> C.Attribute_uniqueness.relax au
       | None -> Log.warning "Not constrained.")
    | `Et_create (etn, allows) ->
      let* et = C.Entity_type.create etn in
      Lwt_list.iter_s (exec_et_adjust et) allows
    | `Et_modify (etn, adjusts) ->
      let* et = req_et etn in
      Lwt_list.iter_s (exec_et_adjust et) adjusts
    | `Et_delete etn ->
      let* et = C.Entity_type.of_name etn in
      Lwt_option.iter_s C.Entity_type.delete et
    | `Et_allow_dsub (etn0, etn1) ->
      let* et0 = req_et etn0 in
      let* et1 = req_et etn1 in
      C.Entity_type.allow_dsub Multiplicity.May Multiplicity.May et0 et1
    | `Et_disallow_dsub (etn0, etn1) ->
      let* et0 = req_et etn0 in
      let* et1 = req_et etn1 in
      C.Entity_type.disallow_dsub et0 et1
    | `Et_allow_attribution (atn, etn0, etn1) ->
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      let* et0 = req_et etn0 in
      let* et1 = req_et etn1 in
      C.Entity_type.allow_attribution at et0 et1
    | `Et_disallow_attribution (atn, etn0, etn1) ->
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      let* et0 = req_et etn0 in
      let* et1 = req_et etn1 in
      C.Entity_type.disallow_attribution at et0 et1
    | `Et_display (etn, template) ->
      let* et = req_et etn in
      C.Entity_type.set_entity_name_tmpl et template
    | `E_force_dsub (sel0, sel1) ->
      let* e0 = Su.select_one sel0 in
      let* e1 = Su.select_one sel1 in
      C.Entity.force_dsub e0 e1
    | `E_relax_dsub (sel0, sel1) ->
      let* e0 = Su.select_one sel0 in
      let* e1 = Su.select_one sel1 in
      C.Entity.relax_dsub e0 e1
    | `E_add_value (atn, vr, sel0, sel1) ->
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      let vt = C.Attribute_type.value_type at in
      let v = Value.typed_of_string vt vr in
      let* e0 = Su.select_one sel0 in
      let* e1 = Su.select_one sel1 in
      let vs = Values.singleton (C.Attribute_type.value_type at) v in
      C.Entity.add_values at vs e0 e1
    | `E_remove_value (atn, vr, sel0, sel1) ->
      let* C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
      let vt = C.Attribute_type.value_type at in
      let v = Value.typed_of_string vt vr in
      let* e0 = Su.select_one sel0 in
      let* e1 = Su.select_one sel1 in
      let vs = Values.singleton (C.Attribute_type.value_type at) v in
      C.Entity.remove_values at vs e0 e1
    | `E_create (sel, etn) ->
      let* et = req_et etn in
      let* e = C.Entity.create et in
      (* TODO: Make sure sel is a path. *)
      add_set_helper `Add e sel
    | `Create (etn, addl) ->
      (C.Entity_type.of_name etn >>= function
       | Some et ->
          let* e = C.Entity.create et in
          Lwt_list.iter_s (exec_mod e) addl
       | None ->
          Subsocia_error.fail_lwt "No entity type is called %s." etn)
    | `Modify (sel, modl) ->
      let* e = Su.select_one sel in
      Lwt_list.iter_s (exec_mod e) modl
    | `Delete sel ->
      let* e = Su.select_one sel in
      C.Entity.delete e

  let exec = Lwt_list.iter_s exec_schema_entry
end
