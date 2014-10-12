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
  open Unprime_list
  open Subsocia_direct
}}

{client{
  module Subsocia = struct
    module Entity = struct
      type id = int32
    end
  end
}}
{server{
  let subsocia_uri = Uri.of_string "postgresql:/"
  module Subsocia = (val Subsocia_direct.connect subsocia_uri)
}}

{shared{
  type entity_info = {
    ei_name : string;
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
    lwt e = Subsocia.Entity.fetch entity_id in
    let label e = Int32.to_string (Subsocia.Entity.get_id e) in
    let mkneigh e = Subsocia.Entity.get_id e, label e in
    lwt preds = Subsocia.Entity.fetch_preds e in
    lwt succs = Subsocia.Entity.fetch_succs e in
    Lwt.return
      { ei_name = label e;
	ei_preds = List.map mkneigh preds;
	ei_succs = List.map mkneigh succs; }
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
    div ~a:[a_class ["entity-browser"]] [
      multicol ~cls:["succ1"] (List.map render_neigh ei.ei_succs);
      div ~a:[a_class ["focus"; "box-top"]] [pcdata ei.ei_name];
      div ~a:[a_class ["focus"; "box-middle"; "content"]] [pcdata "FIXME"];
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
  Main_app.register ~service:Services.main main_handler;
  Main_app.register ~service:Services.entity entity_handler
