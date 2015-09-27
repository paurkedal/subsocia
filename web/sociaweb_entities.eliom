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
  open Lwt.Infix
  open Panograph_i18n
  open Printf
  open Sociaweb_content
  open Sociaweb_services
  open Subsocia_common
  open Unprime
  open Unprime_list
}}

{server{
  open Sociaweb_auth
  open Sociaweb_request
  open Sociaweb_server
  open Subsocia_connection

  let ordered_entities ~cri es =
    let amend_name e =
      lwt name = Entity.display_name ~langs:cri.cri_langs e in
      Lwt.return (name, e) in
    lwt es = Lwt_list.map_s amend_name (Entity.Set.elements es) in
    let es = List.sort (fun (s0, _) (s1, _) -> compare s0 s1) es in
    Lwt.return (List.map snd es)

  let neighbour_link ~cri ent =
    entity_link ~langs:cri.cri_langs ent >|= fun x -> [x]

  let neighbour_with_edit ~cri focus csuper =
    lwt can_edit = Entity.can_edit_entity cri.cri_operator csuper in
    lwt link = entity_link ~langs:cri.cri_langs csuper in
    if not can_edit then
      let button =
	F.button ~button_type:`Button
		 ~a:[F.a_disabled `Disabled; F.a_style "visibility: hidden"]
		 [] in
      Lwt.return (F.td [button; link])
    else
      let focus_id = Entity.id focus in
      let dsuper_id = Entity.id csuper in
      lwt is_dsuper = Entity.is_dsub focus csuper in
      let label, handler, a =
	if is_dsuper then
	  let remove = {{fun _ ->
	    Lwt.async (fun () -> %relax_dsub_sf (%focus_id, %dsuper_id))
	  }} in
	  ("-", remove, None)
	else
	  let add = {{fun _ ->
	    Lwt.async (fun () -> %force_dsub_sf (%focus_id, %dsuper_id))
	  }} in
	  ("+", add, Some [F.a_class ["candidate"]]) in
      let button = F.button ~button_type:`Button ~a:[F.a_onclick handler]
			    [F.pcdata label] in
      Lwt.return (F.td ?a [button; link])

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
    lwt ats = Entity_type.allowed_attributes ubt lbt in
    let ats = Attribute_type.Set.elements ats in
    let render_tr (Attribute_type.Ex at) =
      lwt an = Attribute_type.name at in
      let mu = Attribute_type.value_mult at in
      lwt value_frag =
	let t1 = Attribute_type.value_type at in
	Entity.get_values at ub lb >|= fun vs ->
	match List.map (Value.typed_to_string t1) (Values.elements vs) with
	| [] -> [F.span ~a:[F.a_class ["none"]] [F.pcdata "-"]]
	| strs -> [F.pcdata (String.concat ", " strs)] in
      Lwt.return @@ F.tr [
	F.td [F.pcdata an; F.sup [F.pcdata (Multiplicity.to_string mu)];
	      F.pcdata ":"];
	F.td value_frag;
      ] in
    lwt attr_trs = Lwt_list.map_s render_tr ats in
    lwt ub_name = Entity.display_name ~langs:cri.cri_langs ub in
    Lwt.return
      (if attr_trs = []
       then None
       else Some (F.tr [F.td []; F.th [F.pcdata ub_name]] :: attr_trs))

  let render_dsuper ~cri ~enable_edit focus =
    let is_relevant csuper =
      lwt is_dsuper = Entity.is_dsub focus csuper in
      if is_dsuper then Lwt.return_true else
      Entity.can_edit_entity cri.cri_operator csuper in
    if enable_edit then
      lwt csupers = Entity.candidate_dsupers ~include_current:true focus in
      lwt csupers = Entity.Set.filter_s is_relevant csupers in
      lwt csupers = ordered_entities ~cri csupers in
      lwt csuper_frags = Lwt_list.map_s (neighbour_with_edit ~cri focus)
					csupers in
      let csuper_block = multicol_tds ~cls:["soc-dsuper1"] csuper_frags in
      Lwt.return @@
	F.table ~a:[F.a_class ["soc-layout"]]
	  [F.tr [F.th [F.pcdata "Membership Management"]];
	   F.tr [F.td [csuper_block]]]
    else
      lwt dsupers = Entity.dsuper focus in
      lwt dsupers = ordered_entities ~cri dsupers in
      lwt dsuper_frags = Lwt_list.map_s (neighbour_link ~cri) dsupers in
      let dsuper_block = multicol ~cls:["soc-dsuper1"] dsuper_frags in
      Lwt.return @@
	F.table ~a:[F.a_class ["soc-layout"]]
	  [F.tr [F.th [F.pcdata "Member of"]];
	   F.tr [F.td [dsuper_block]]]

  let render_browser ~cri ?(enable_edit = true) ent =
    let open Html5 in
    lwt dsub = Entity.dsub ent >>= ordered_entities ~cri in
    lwt dsub_frags = Lwt_list.map_s (neighbour_link ~cri) dsub in
    lwt name = Entity.display_name ~langs:cri.cri_langs ent in
    lwt ubs = upwards_closure ent in
    let attr_aux ub acc =
      render_attribution ~cri ent ub
	>|= function None -> acc | Some trs -> trs :: acc in
    lwt attr_trss = Entity.Set.fold_s attr_aux ubs [] in
    let attr_table = F.table ~a:[F.a_class ["soc-assoc"]]
			     (List.flatten attr_trss) in
    lwt dsuper_frag = render_dsuper ~cri ~enable_edit ent in
    Lwt.return @@ F.div ~a:[F.a_class ["soc-entity-browser"]] [
      dsuper_frag;
      F.div ~a:[F.a_class ["soc-box"; "focus"; "top"]] [F.pcdata name];
      F.div ~a:[F.a_class ["soc-box"; "focus"; "middle"; "content"]]
	    [attr_table];
      F.div ~a:[F.a_class ["soc-box"; "focus"; "bottom"; "content"]]
	    [multicol ~cls:["soc-dsub1"] dsub_frags];
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

let () =
  Eliom_registration.Redirection.register ~service:self_entity_service
					  self_entity_handler;
  App.register ~service:entity_service entity_handler