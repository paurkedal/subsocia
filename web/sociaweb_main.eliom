(* Copyright (C) 2014--2015  Petter A. Urkedal <paurkedal@gmail.com>
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

  let listing ?(cls = []) frags = F.p ~a:[F.a_class cls] (List.map F.span frags)

  let entity_for_view ~operator entity_id =
    lwt entity = Entity.of_id entity_id in
    match_lwt Entity.can_view_entity operator entity with
    | true -> Lwt.return entity
    | false -> http_error 403 "Not authorized for this entity."

  let entity_for_edit ~operator entity_id =
    lwt entity = Entity.of_id entity_id in
    match_lwt Entity.can_edit_entity operator entity with
    | true -> Lwt.return entity
    | false -> http_error 403 "Not authorized for editing this entity."

  let force_dsub ~operator (lb_id, ub_id) =
    lwt ub = entity_for_edit ~operator ub_id in
    lwt lb = entity_for_view ~operator lb_id in
    lwt user_name = Entity.display_name ~langs:[] operator in
    Lwt_log.info_f "%s adds inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
    Entity.force_dsub lb ub
  let constrain_c = auth_sf Json.t<int32 * int32> force_dsub

  let relax_dsub ~operator (lb_id, ub_id) =
    lwt ub = entity_for_edit ~operator ub_id in
    lwt lb = entity_for_view ~operator lb_id in
    lwt user_name = Entity.display_name ~langs:[] operator in
    Lwt_log.info_f "%s removes inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
    Entity.relax_dsub lb ub
  let unconstrain_c = auth_sf Json.t<int32 * int32> relax_dsub

  let neighbour_link ~cri ent =
    entity_link ~langs:cri.cri_langs ent >|= fun x -> [x]

  let neighbour_with_remove ~cri focus dsuper =
    lwt can_edit = Entity.can_edit_entity cri.cri_operator dsuper in
    let focus_id = Entity.id focus in
    let dsuper_id = Entity.id dsuper in
    lwt link = entity_link ~langs:cri.cri_langs dsuper in
    if can_edit then begin
      let on_remove = {{fun _ ->
	Eliom_lib.debug "Removing.";
	Lwt.async (fun () -> %unconstrain_c (%focus_id, %dsuper_id))
      }} in
      Lwt.return [
	link; F.pcdata " ";
	F.button ~button_type:`Button ~a:[F.a_onclick on_remove] [F.pcdata "−"];
      ]
    end else
      Lwt.return [link]

  let neighbour_with_add ~cri focus dsuper =
    lwt can_edit = Entity.can_edit_entity cri.cri_operator dsuper in
    let focus_id = Entity.id focus in
    let dsuper_id = Entity.id dsuper in
    lwt link = entity_link ~langs:cri.cri_langs dsuper in
    if can_edit then begin
      let on_add = {{fun _ ->
	Eliom_lib.debug "Adding.";
	Lwt.async (fun () -> %constrain_c (%focus_id, %dsuper_id))
      }} in
      Lwt.return [
	link; F.pcdata " ";
	F.button ~button_type:`Button ~a:[F.a_onclick on_add] [F.pcdata "+"];
      ]
    end else
      Lwt.return [link]

  let rec fold_closure_from f dsucc x acc =
    lwt xs = dsucc x in
    lwt acc = Entity.Set.fold_s (fold_closure_from f dsucc) xs acc in
    Lwt.return (f x acc)

  let upwards_closure e =
    fold_closure_from Entity.Set.add Entity.dsuper e Entity.Set.empty

  let render_attribution ~cri lb ub =
    let open Html5 in
    lwt lbt = Entity.type_ lb in
    lwt ubt = Entity.type_ ub in
    lwt attrs = Entity_type.can_asub_byattr lbt ubt in
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

  let render_dsuper ~cri ~enable_edit ent =
    let neighbour =
      if enable_edit then neighbour_with_remove ent
		     else neighbour_link in
    lwt dsupers = Entity.dsuper ent in
    let dsupers' = Entity.Set.elements dsupers in
    lwt dsuper_frags = Lwt_list.map_s (neighbour ~cri) dsupers' in
    let dsuper_block = listing ~cls:["dsuper1"] dsuper_frags in
    lwt dsuper_add_block =
      if not enable_edit then Lwt.return_none else
      let operator = cri.cri_operator in
      lwt csupers = Entity.candidate_dsupers ent in
      let csupers = Entity.Set.compl dsupers csupers in
      lwt csupers = Entity.Set.filter_s (Entity.can_edit_entity operator)
					csupers in
      if Entity.Set.is_empty csupers then
	Lwt.return_none
      else
	let csupers = Entity.Set.elements csupers in
	lwt csupers = Lwt_list.map_s (neighbour_with_add ~cri ent) csupers in
	Lwt.return (Some (listing ~cls:["candidate"; "dsuper1"] csupers)) in
    Lwt.return @@
      match dsuper_add_block with
      | None ->
	F.table ~a:[F.a_class ["layout"]]
	  [F.tr [F.th [F.pcdata "Member of"]];
	   F.tr [F.td [dsuper_block]]]
      | Some dsuper_add_block ->
	F.table ~a:[F.a_class ["layout"]]
	  [F.tr [F.th [F.pcdata "Member of"]; F.th [F.pcdata "Not member of"]];
	   F.tr [F.td [dsuper_block]; F.td [dsuper_add_block]]]

  let render_browser ~cri ?(enable_edit = true) ent =
    let open Html5 in
    lwt dsub = Entity.dsub ent in
    let dsub' = Entity.Set.elements dsub in
    lwt dsub_frags = Lwt_list.map_s (neighbour_link ~cri) dsub' in
    lwt name = Entity.display_name ~langs:cri.cri_langs ent in
    lwt ubs = upwards_closure ent in
    let attr_aux ub acc =
      render_attribution ~cri ent ub
	>|= function None -> acc | Some trs -> trs :: acc in
    lwt attr_trss = Entity.Set.fold_s attr_aux ubs [] in
    let attr_table = F.table ~a:[F.a_class ["assoc"]]
			     (List.flatten attr_trss) in
    lwt dsuper_frag = render_dsuper ~cri ~enable_edit ent in
    Lwt.return @@ F.div ~a:[F.a_class ["entity-browser"]] [
      dsuper_frag;
      F.div ~a:[F.a_class ["focus"; "box-top"]] [F.pcdata name];
      F.div ~a:[F.a_class ["focus"; "box-middle"; "content"]] [attr_table];
      F.div ~a:[F.a_class ["focus"; "box-bottom"; "content"]] [
	listing ~cls:["dsub1"] dsub_frags;
      ];
    ]
}}

(* TODO: Set enable_edit from permissions or explicit request. *)

let entity_handler entity_id () =
  let open Html5.D in
  lwt cri = get_custom_request_info () in
  lwt e = entity_for_view ~operator:cri.cri_operator entity_id in
  lwt browser =
    render_browser ~cri e in
  let entity_changed_c = Eliom_react.Down.of_react (entity_changed e) in
  ignore {unit{
    Lwt_react.E.keep @@ React.E.trace
      (fun _ ->
	Eliom_client.exit_to ~service:Eliom_service.void_coservice' () ())
      %entity_changed_c
  }};
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
