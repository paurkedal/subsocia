(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

{shared{
  open Eliom_content
  open Eliom_content.Html5
  open Panograph_i18n
  open Printf
  open Sociaweb_content
  open Sociaweb_services
  open Subsocia_common
  open Unprime
  open Unprime_list

  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
}}

{server{
  open Sociaweb_auth
  open Sociaweb_request
  open Subsocia_connection

  let can_edit_entity ~operator entity =
    lwt admin = Entity.admin entity in
    Entity.precedes operator admin

  let entity_for_view ~operator entity_id =
    Entity.of_id entity_id (* TODO: View permissions. *)

  let entity_for_edit ~operator entity_id =
    lwt entity = Entity.of_id entity_id in
    lwt can_edit = can_edit_entity ~operator entity in
    if can_edit then Lwt.return entity
		else http_error 403 "Unauthorized edit."

  let constrain ~operator (lb_id, ub_id) =
    lwt ub = entity_for_edit ~operator ub_id in
    lwt lb = entity_for_view ~operator lb_id in
    lwt user_name = Entity.display_name ~langs:[] operator in
    Lwt_log.info_f "%s adds inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
    Entity.constrain lb ub
  let constrain_c = auth_sf Json.t<int32 * int32> constrain

  let unconstrain ~operator (lb_id, ub_id) =
    lwt ub = entity_for_edit ~operator ub_id in
    lwt lb = entity_for_view ~operator lb_id in
    lwt user_name = Entity.display_name ~langs:[] operator in
    Lwt_log.info_f "%s removes inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
    Entity.unconstrain lb ub
  let unconstrain_c = auth_sf Json.t<int32 * int32> unconstrain

  let render_neigh ~cri ent =
    let id = Entity.id ent in
    lwt name = Entity.display_name ~langs:cri.cri_langs ent in
    Lwt.return [F.a ~service:entity_service [F.pcdata name] id]

  let render_neigh_remove ~cri focus succ =
    lwt succ_admin = Entity.admin succ in
    lwt can_edit = Entity.precedes cri.cri_operator succ_admin in
    let focus_id = Entity.id focus in
    let succ_id = Entity.id succ in
    lwt name = Entity.display_name ~langs:cri.cri_langs succ in
    let link = F.a ~service:entity_service [F.pcdata name] succ_id in
    if can_edit then begin
      let on_remove = {{fun _ ->
	Eliom_lib.debug "Removing.";
	Lwt.async @@ fun () ->
	%unconstrain_c (%focus_id, %succ_id)
      }} in
      Lwt.return [
	link; F.pcdata " ";
	F.button ~button_type:`Button ~a:[F.a_onclick on_remove] [F.pcdata "−"];
      ]
    end else
      Lwt.return [link]

  let render_neigh_add ~cri focus succ =
    lwt succ_admin = Entity.admin succ in
    lwt can_edit = Entity.precedes cri.cri_operator succ_admin in
    let focus_id = Entity.id focus in
    let succ_id = Entity.id succ in
    lwt name = Entity.display_name ~langs:cri.cri_langs succ in
    let link = F.a ~service:entity_service [F.pcdata name] succ_id in
    if can_edit then begin
      let on_add = {{fun _ ->
	Eliom_lib.debug "Adding.";
	Lwt.async @@ fun () ->
	%constrain_c (%focus_id, %succ_id)
      }} in
      Lwt.return [
	link; F.pcdata " ";
	F.button ~button_type:`Button ~a:[F.a_onclick on_add] [F.pcdata "+"];
      ]
    end else
      Lwt.return [link]

  let rec fold_closure_from f succs x acc =
    lwt xs = succs x in
    lwt acc = Entity.Set.fold_s (fold_closure_from f succs) xs acc in
    Lwt.return (f x acc)

  let upwards_closure e =
    fold_closure_from Entity.Set.add Entity.succs e Entity.Set.empty

  let render_attribution ~cri lb ub =
    let open Html5 in
    lwt lbt = Entity.type_ lb in
    lwt ubt = Entity.type_ ub in
    lwt attrs = Entity_type.attribution lbt ubt in
    let attrs' = Attribute_type.Map.bindings attrs in
    let render_tr (at, mu) =
      lwt an = Attribute_type.name at in
      lwt value_frag =
	let Attribute_type.Ex at1 = at in
	let t1 = Attribute_type.type1 at1 in
	Entity.getattr lb ub at1 >|= fun vs ->
	match List.map (Value.typed_to_string t1) (Values.elements vs) with
	| [] -> [F.span ~a:[F.a_class ["none"]] [F.pcdata "-"]]
	| strs -> [F.pcdata (String.concat ", " strs)] in
      Lwt.return @@ F.tr [
	F.td [F.pcdata an; F.sup [F.pcdata (Multiplicity.to_string mu)];
	      F.pcdata ":"];
	F.td value_frag;
      ] in
    lwt attr_trs = Lwt_list.map_s render_tr attrs' in
    lwt ub_name = Entity.display_name ~langs:cri.cri_langs ub in
    Lwt.return
      (if attr_trs = []
       then None
       else Some (F.tr [F.td []; F.th [F.pcdata ub_name]] :: attr_trs))

  let render_succs ~cri ~enable_edit ent =
    let render_succ =
      if enable_edit then render_neigh_remove ent
		     else render_neigh in
    lwt succs = Entity.succs ent in
    let succs' = Entity.Set.elements succs in
    lwt succ_frags = Lwt_list.map_s (render_succ ~cri) succs' in
    let succs_view = multicol ~cls:["succ1"] succ_frags in
    lwt succs_add =
      if not enable_edit then Lwt.return_none else
      let operator = cri.cri_operator in
      lwt csuccs = Entity.candidate_succs ent in
      let csuccs = Entity.Set.compl succs csuccs in
      lwt csuccs = Entity.Set.filter_s (can_edit_entity ~operator) csuccs in
      if Entity.Set.is_empty csuccs then
	Lwt.return_none
      else
	let csuccs = Entity.Set.elements csuccs in
	lwt csuccs = Lwt_list.map_s (render_neigh_add ~cri ent) csuccs in
	Lwt.return (Some (multicol ~cls:["candidate"; "succ1"] csuccs)) in
    Lwt.return @@
      match succs_add with
      | None ->
	F.table ~a:[F.a_class ["layout"]]
	  [F.tr [F.th [F.pcdata "Member of"]];
	   F.tr [F.td [succs_view]]]
      | Some succs_add ->
	F.table ~a:[F.a_class ["layout"]]
	  [F.tr [F.th [F.pcdata "Member of"]; F.th [F.pcdata "Not member of"]];
	   F.tr [F.td [succs_view]; F.td [succs_add]]]

  let render_browser ~cri ?(enable_edit = true) ent =
    let open Html5 in
    lwt preds = Entity.preds ent in
    let preds' = Entity.Set.elements preds in
    lwt pred_frags = Lwt_list.map_s (render_neigh ~cri) preds' in
    lwt name = Entity.display_name ~langs:cri.cri_langs ent in
    lwt ubs = upwards_closure ent in
    let attr_aux ub acc =
      render_attribution ~cri ent ub
	>|= function None -> acc | Some trs -> trs :: acc in
    lwt attr_trss = Entity.Set.fold_s attr_aux ubs [] in
    let attr_table = F.table ~a:[F.a_class ["assoc"]]
			     (List.flatten attr_trss) in
    lwt succs_frag = render_succs ~cri ~enable_edit ent in
    Lwt.return @@ F.div ~a:[F.a_class ["entity-browser"]] [
      succs_frag;
      F.div ~a:[F.a_class ["focus"; "box-top"]] [F.pcdata name];
      F.div ~a:[F.a_class ["focus"; "box-middle"; "content"]] [attr_table];
      F.div ~a:[F.a_class ["focus"; "box-bottom"; "content"]] [
	multicol ~cls:["pred1"] pred_frags;
      ];
    ]
}}

(* TODO: Set enable_edit from permissions or explicit request. *)

let entity_handler entity_id () =
  let open Html5.D in
  lwt cri = get_custom_request_info () in
  lwt browser =
    lwt e = Entity.of_id entity_id in
    render_browser ~cri e in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Entity Browser"
      ~css:[["css"; "subsocia.css"]]
      (body [browser])

let self_entity_handler () () =
  lwt operator = get_operator () in
  let operator_id = Entity.id operator in
  Lwt.return (Eliom_service.preapply entity_service operator_id)

module Main_app =
  Eliom_registration.App (struct let application_name = "sociaweb_main" end)

let () =
  Subsocia_plugin.load_web_plugins ();
  Eliom_registration.Redirection.register ~service:self_entity_service
					  self_entity_handler;
  Main_app.register ~service:entity_service entity_handler
