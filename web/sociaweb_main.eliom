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
  open Sociaweb_server

  let can_edit_entity ~user entity =
    lwt admin = Sc.Entity.admin entity in
    Sc.Entity.precedes user admin

  let entity_for_view ~user entity_id =
    Sc.Entity.of_id entity_id (* TODO: View permissions. *)

  let entity_for_edit ~user entity_id =
    lwt entity = Sc.Entity.of_id entity_id in
    lwt can_edit = can_edit_entity ~user entity in
    if can_edit then Lwt.return entity
		else http_error 403 "Unauthorized edit."

  let constrain ~user (lb_id, ub_id) =
    lwt ub = entity_for_edit ~user ub_id in
    lwt lb = entity_for_view ~user lb_id in
    lwt user_name = Scd.Entity.display_name ~langs:[] user in
    Lwt_log.info_f "%s adds inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
    Sc.Entity.constrain lb ub
  let constrain_c = auth_sf Json.t<int32 * int32> constrain

  let unconstrain ~user (lb_id, ub_id) =
    lwt ub = entity_for_edit ~user ub_id in
    lwt lb = entity_for_view ~user lb_id in
    lwt user_name = Scd.Entity.display_name ~langs:[] user in
    Lwt_log.info_f "%s removes inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
    Sc.Entity.unconstrain lb ub
  let unconstrain_c = auth_sf Json.t<int32 * int32> unconstrain

  let render_neigh ~langs ent =
    let id = Sc.Entity.id ent in
    lwt name = Scd.Entity.display_name ~langs ent in
    Lwt.return [F.a ~service:entity_service [F.pcdata name] id]

  let render_neigh_remove ~langs focus succ =
    let focus_id = Sc.Entity.id focus in
    let succ_id = Sc.Entity.id succ in
    lwt name = Scd.Entity.display_name ~langs succ in
    let on_remove = {{fun _ ->
      Eliom_lib.debug "Removing.";
      Lwt.async @@ fun () ->
      %unconstrain_c (%focus_id, %succ_id)
    }} in
    Lwt.return [
      F.a ~service:entity_service [F.pcdata name] succ_id;
      F.pcdata " ";
      F.button ~button_type:`Button ~a:[F.a_onclick on_remove] [F.pcdata "−"];
    ]

  let render_neigh_add ~langs focus succ =
    let focus_id = Sc.Entity.id focus in
    let succ_id = Sc.Entity.id succ in
    lwt name = Scd.Entity.display_name ~langs succ in
    let on_add = {{fun _ ->
      Eliom_lib.debug "Adding.";
      Lwt.async @@ fun () ->
      %constrain_c (%focus_id, %succ_id)
    }} in
    Lwt.return [
      F.a ~service:entity_service [F.pcdata name] succ_id;
      F.pcdata " ";
      F.button ~button_type:`Button ~a:[F.a_onclick on_add] [F.pcdata "+"];
    ]

  let rec fold_s_closure_from f succs x acc =
    lwt xs = succs x in
    lwt acc = Lwt_list.fold_right_s (fold_s_closure_from f succs) xs acc in
    f x acc

  let map_s_closure_from f succs x =
    fold_s_closure_from (fun x acc -> f x >|= fun x -> x :: acc) succs x []

  let render_attribution ~langs lb ub =
    let open Html5 in
    lwt lbt = Sc.Entity.type_ lb in
    lwt ubt = Sc.Entity.type_ ub in
    lwt attrs = Sc.Entity_type.attribution lbt ubt in
    let attrs' = Sc.Attribute_type.Map.bindings attrs in
    let render_tr (at, mu) =
      lwt an = Sc.Attribute_type.name at in
      lwt value_frag =
	let Sc.Attribute_type.Ex at1 = at in
	let t1 = Sc.Attribute_type.type1 at1 in
	Sc.Entity.getattr lb ub at1 >|= fun vs ->
	match List.map (Value.typed_to_string t1) (Values.elements vs) with
	| [] -> [F.span ~a:[F.a_class ["none"]] [F.pcdata "-"]]
	| strs -> [F.pcdata (String.concat ", " strs)] in
      Lwt.return @@ F.tr [
	F.td [F.pcdata an; F.sup [F.pcdata (Multiplicity.to_string mu)];
	      F.pcdata ":"];
	F.td value_frag;
      ] in
    lwt attr_trs = Lwt_list.map_s render_tr attrs' in
    lwt ub_name = Scd.Entity.display_name ~langs ub in
    Lwt.return
      (if attr_trs = []
       then None
       else Some (F.tr [F.td []; F.th [F.pcdata ub_name]] :: attr_trs))

  let render_browser ~langs ?(enable_edit = true) ent =
    let open Html5 in
    lwt succs = Sc.Entity.succs ent in
    lwt preds = Sc.Entity.preds ent in
    let succs', preds' = Sc.Entity.Set.(elements succs, elements preds) in
    let render_succ =
      if enable_edit then render_neigh_remove ent
		     else render_neigh in
    lwt succ_frags = Lwt_list.map_s (render_succ ~langs) succs' in
    lwt pred_frags = Lwt_list.map_s (render_neigh ~langs) preds' in
    lwt edit_succs =
      if not enable_edit then Lwt.return (F.pcdata "") else
      lwt candidate_succs = Scd.Entity.candidate_succs ent in
      let candidate_succs = Sc.Entity.Set.compl succs candidate_succs in
      let candidate_succs' = Sc.Entity.Set.elements candidate_succs in
      lwt candidate_succ_frags = Lwt_list.map_s (render_neigh_add ent ~langs)
				 candidate_succs' in
      Lwt.return (multicol ~cls:["candidate"; "succ1"] candidate_succ_frags) in
    lwt name = Scd.Entity.display_name ~langs ent in
    lwt attr_trss =
      map_s_closure_from (render_attribution ~langs ent)
	  (fun ent -> Sc.Entity.succs ent >|= Sc.Entity.Set.elements) ent in
    let attr_table = F.table ~a:[F.a_class ["assoc"]]
			     (List.flatten (List.fmap ident attr_trss)) in
    Lwt.return @@ F.div ~a:[F.a_class ["entity-browser"]] [
      edit_succs;
      multicol ~cls:["succ1"] succ_frags;
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
  lwt auth_entity = auth_entity () in
  let langs = [Lang.of_string "en"] in
  lwt browser =
    lwt e = Sc.Entity.of_id entity_id in
    render_browser ~langs e in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Entity Browser"
      ~css:[["css"; "subsocia.css"]]
      (body [browser])

let main_handler () () =
  lwt auth_entity = auth_entity () in
  let auth_entity_id = Sc.Entity.id auth_entity in
  Lwt.return (Eliom_service.preapply entity_service auth_entity_id)

module Main_app =
  Eliom_registration.App (struct let application_name = "sociaweb_main" end)
let () =
  Eliom_registration.Redirection.register ~service:main_service main_handler;
  Main_app.register ~service:entity_service entity_handler
