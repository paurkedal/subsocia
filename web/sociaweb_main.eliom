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
  open Panograph_i18n
  open Printf
  open Sociaweb_content
  open Subsocia_common
  open Unprime
  open Unprime_list
  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)

  module Services = struct
    open Eliom_service
    open Eliom_parameter
    let main = App.service ~path:[] ~get_params:unit ()
    let entity =
      App.service ~path:["entities"] ~get_params:(suffix (int32 "entity_id")) ()
  end
}}

{server{

  open Subsocia_common
  let langs = [Lang.of_string "en"]
  let subsocia_uri = Uri.of_string "postgresql:/"
  module Socia = (val Subsocia_direct.connect subsocia_uri)
  let process = Subsocia_rpc_server.process (module Socia)

  let call = server_function Json.t<string> @@ fun s ->
    Lwt_log.debug_f "RPC request: %s" s >>
    process (Jsonrpc.call_of_string s) >|= Jsonrpc.string_of_response

  module SC = Socia
  module Config = struct
    let display_name_attributes = Subsocia_config.display_name#get
  end
  module SU = Subsocia_derived.Make (Config) (SC)
}}

(*
{client{ (* Needed if next is client|shared. *)
  module SC = Subsocia_rpc_client.Make
    (struct
      type 'a t = 'a Lwt.t
      let bind = Lwt.bind
      let return = Lwt.return
      let fail = Lwt.fail
      let rpc c =
	%call (Jsonrpc.string_of_call c) >|= Jsonrpc.response_of_string
    end)
  module Config = struct
    let display_name_attributes = %(Subsocia_config.display_name#get)
  end
  module SU = Subsocia_derived.Make (Config) (SC)
}}
*)

{server{ (* client|server|shared *)

  let render_neigh ~langs ent =
    let open Html5 in
    let id = SC.Entity.id ent in
    lwt name = SU.Entity.display_name ~langs ent in
    Lwt.return [F.a ~service:Services.entity [F.pcdata name] id]

  let rec fold_s_closure_from f succs x acc =
    lwt xs = succs x in
    lwt acc = Lwt_list.fold_right_s (fold_s_closure_from f succs) xs acc in
    f x acc

  let map_s_closure_from f succs x =
    fold_s_closure_from (fun x acc -> f x >|= fun x -> x :: acc) succs x []

  let render_attribution ~langs lb ub =
    let open Html5 in
    lwt lbt = SC.Entity.type_ lb in
    lwt ubt = SC.Entity.type_ ub in
    lwt attrs = SC.Entity_type.attribution lbt ubt in
    let attrs' = SC.Attribute_type.Map.bindings attrs in
    let render_tr (at, mu) =
      lwt an = SC.Attribute_type.name at in
      lwt value_frag =
	let SC.Attribute_type.Ex at1 = at in
	let t1 = SC.Attribute_type.type1 at1 in
	SC.Entity.getattr lb ub at1 >|= fun vs ->
	match List.map (Value.typed_to_string t1) (Values.elements vs) with
	| [] -> [F.span ~a:[F.a_class ["none"]] [F.pcdata "-"]]
	| strs -> [F.pcdata (String.concat ", " strs)] in
      Lwt.return @@ F.tr [
	F.td [F.pcdata an; F.sup [F.pcdata (Multiplicity.to_string mu)];
	      F.pcdata ":"];
	F.td value_frag;
      ] in
    lwt attr_trs = Lwt_list.map_s render_tr attrs' in
    lwt ub_name = SU.Entity.display_name ~langs ub in
    Lwt.return
      (if attr_trs = []
       then None
       else Some (F.tr [F.td []; F.th [F.pcdata ub_name]] :: attr_trs))

  let render_browser ~langs ent =
    let open Html5 in
    lwt succs = SC.Entity.succs ent in
    lwt preds = SC.Entity.preds ent in
    let succs', preds' = SC.Entity.Set.(elements succs, elements preds) in
    lwt succ_frags = Lwt_list.map_s (render_neigh ~langs) succs' in
    lwt pred_frags = Lwt_list.map_s (render_neigh ~langs) preds' in
    lwt name = SU.Entity.display_name ~langs ent in
    lwt attr_trss =
      map_s_closure_from (render_attribution ~langs ent)
	  (fun ent -> SC.Entity.succs ent >|= SC.Entity.Set.elements) ent in
    let attr_table = F.table ~a:[F.a_class ["assoc"]]
			     (List.flatten (List.fmap ident attr_trss)) in
    Lwt.return @@ F.div ~a:[F.a_class ["entity-browser"]] [
      multicol ~cls:["succ1"] succ_frags;
      F.div ~a:[F.a_class ["focus"; "box-top"]] [F.pcdata name];
      F.div ~a:[F.a_class ["focus"; "box-middle"; "content"]] [attr_table];
      F.div ~a:[F.a_class ["focus"; "box-bottom"; "content"]] [
	multicol ~cls:["pred1"] pred_frags;
      ];
    ]
}}

let entity_handler entity_id () =
  let open Html5.D in
  let langs = [Lang.of_string "en"] in
  lwt browser =
    lwt e = SC.Entity.of_id entity_id in
    render_browser ~langs e in
(*
  let browser = client_node_lwt {{
    lwt ei = SC.Entity.of_id %entity_id in
    render_browser ~langs:%langs ei
  }} in
*)
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Entity Browser"
      ~css:[["css"; "subsocia.css"]]
      (body [browser])

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
