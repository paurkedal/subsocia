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
  open Printf
  open Unprime_list
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
}}

{client{
  module Subsocia = struct
    module Entity = struct
      type id = int32
    end
  end
}}
{server{
  open Subsocia_common
  open Subsocia_direct
  let langs = [lang_of_string "en"]
  let subsocia_uri = Uri.of_string "postgresql:/"
  module Subsocia = (val Subsocia_direct.connect subsocia_uri)
}}

{shared{
  type entity_info = {
    ei_name : string;
    ei_attributes : (string * string) list;
    ei_preds : (Subsocia.Entity.id * string) list;
    ei_succs : (Subsocia.Entity.id * string) list;
  }

  module Services = struct
    open Eliom_service
    open Eliom_parameter
    let main = App.service ~path:[] ~get_params:unit ()
    let entity =
      App.service ~path:["entities"] ~get_params:(suffix (int32 "entity_id")) ()
  end
}}

{server{
  let fetch_entity entity_id =
    let label e =
      lwt en = Subsocia.Entity.fetch_display_name ~langs e in
      lwt et = Subsocia.Entity.fetch_type e in
      let tn = Subsocia.Entity_type.get_display_name ~langs et in
      Lwt.return (sprintf "%s : %s" en tn) in
    lwt e = Subsocia.Entity.fetch entity_id in
    lwt et = Subsocia.Entity.fetch_type e in
    let mkneigh e = label e >|= fun l -> Subsocia.Entity.get_id e, l in
    lwt preds = Subsocia.Entity.fetch_preds e in
    lwt succs = Subsocia.Entity.fetch_succs e in
    lwt ei_name = label e in
    let fetchattr ai =
      match ai.ai_key with
      | Exists_attribute_key ((_, at) as ak) ->
	lwt av = Subsocia.Entity.fetch_attribute e ak in
	Lwt.return (Twine.to_string ~langs ai.ai_name,
		    string_of_attribute ~langs at av) in
    let module P = (val Subsocia.Entity_type.get_plugin et) in
    lwt ei_attributes = Lwt_list.map_s fetchattr P.attributes in
    lwt ei_preds = Lwt_list.map_s mkneigh preds in
    lwt ei_succs = Lwt_list.map_s mkneigh succs in
    Lwt.return {ei_name; ei_attributes; ei_preds; ei_succs}
}}

{client{
  let multicol ?(m = 4) ?(cls = []) items =
    let open Html5.F in
    let items = Array.of_list items in
    let l = Array.length items in
    let n = (l + m - 1) / m in
    let mktr i =
      let mktd j = td (if i + j * n < l then items.(i + j * n) else []) in
      tr (List.sample mktd m) in
    match List.sample mktr n with
    | [] -> div ~a:[a_class ("multicol" :: "empty" :: cls)] []
    | trs -> table ~a:[a_class ("multicol" :: cls)] trs

  let render_neigh (id, l) =
    let open Html5.F in
    [a ~service:Services.entity [pcdata l] id]

  let render_browser ei =
    let open Html5.F in
    let render_attribute (k, v) = tr [th [pcdata k]; td [pcdata v]] in
    div ~a:[a_class ["entity-browser"]] [
      multicol ~cls:["succ1"] (List.map render_neigh ei.ei_succs);
      div ~a:[a_class ["focus"; "box-top"]] [pcdata ei.ei_name];
      div ~a:[a_class ["focus"; "box-middle"; "content"]] [
	table ~a:[a_class ["assoc"]]
	      (List.map render_attribute ei.ei_attributes)
      ];
      div ~a:[a_class ["focus"; "box-bottom"; "content"]] [
	multicol ~cls:["pred1"] (List.map render_neigh ei.ei_preds);
      ];
    ];
}}

let entity_handler entity_id () =
  let open Html5.D in
  lwt ei = fetch_entity entity_id in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Entity Browser"
      ~css:[["css"; "subsocia.css"]]
      (body [
	Html5.C.node {{render_browser %ei}};
      ])

let main_handler () () =
  let open Html5.D in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"TODO: Write the Application"
      ~css:[["css"; "subsocia.css"]]
      (body [
	h1 [pcdata "TODO: Write the Application"];
      ])

module Main_app =
  Eliom_registration.App (struct let application_name = "subsocia_web" end)
let () =
  ignore Subsocia_direct_plugins.registered;
  Main_app.register ~service:Services.main main_handler;
  Main_app.register ~service:Services.entity entity_handler
