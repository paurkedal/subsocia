(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

open Eliom_content
open Eliom_content.Html5
open Sociaweb_services
open Sociaweb_server

let registration_form (first_name, (last_name, email)) =
  [F.table ~a:[F.a_class ["assoc"]] [
    F.tr [
      F.th [F.pcdata "First name:"];
      F.td [F.string_input ~input_type:`Text ~name:first_name ()];
    ];
    F.tr [
      F.th [F.pcdata "Last name:"];
      F.td [F.string_input ~input_type:`Text ~name:last_name ()];
    ];
    F.tr [
      F.th [F.pcdata "Email:"];
      F.td [F.string_input ~input_type:`Email ~name:email ()];
    ];
    F.tr [
      F.td [];
      F.td [F.string_input ~input_type:`Submit ~value:"Register" ()];
    ];
  ]]

let () =
  Eliom_registration.Html5.register ~service:registration_form_service
    @@ fun () () ->
  Lwt.return @@
    Eliom_tools.F.html
      ~title:"Registration"
      ~css:[["css"; "subsocia.css"]]
      (F.body [
	F.h1 [F.pcdata "Registration"];
	F.post_form ~service:registration_post_service registration_form ();
      ])

let () =
  Eliom_registration.Html5.register ~service:registration_post_service
    @@ fun () (first_name, (last_name, email)) ->
  begin match_lwt auth_entity_opt () with
  | None -> Lwt.return_unit
  | Some _ -> http_error 400 "Already registered."
  end >>
  lwt identity = auth_identity () in
  lwt e_auth_group = Sociaweb_server.e_auth_group in
  lwt at_unique_name = Scd.Const.at_unique_name in
  lwt at_first_name = Scd.Const.at_first_name in
  lwt at_last_name = Scd.Const.at_last_name in
  lwt at_email = Scd.Const.at_email in
  lwt e_unit = Scd.Const.e_unit in
  lwt e_viewer = Scd.Const.e_default_viewers in
  lwt e_admin = Scd.Const.e_default_admins in
  lwt et_person = Scd.Const.et_person in
  lwt e_new_user = Sc.Entity.create ~viewer:e_viewer ~admin:e_admin et_person in
  lwt e_new_users = Scd.Const.e_new_users in
  Sc.Entity.constrain e_new_user e_new_users >>
  Sc.Entity.setattr e_new_user e_auth_group at_unique_name [identity] >>
  Sc.Entity.setattr e_new_user e_unit at_first_name [first_name] >>
  Sc.Entity.setattr e_new_user e_unit at_last_name [last_name] >>
  Sc.Entity.setattr e_new_user e_unit at_email [email] >>
  Lwt.return @@
    Eliom_tools.F.html
      ~title:"Welcome"
      ~css:[["css"; "subsocia.css"]]
      (F.body [
	F.h1 [F.pcdata "Welcome, "; F.pcdata first_name];
	F.p [
	  F.a ~service:entity_service
	    [F.pcdata "Your registration is complete."]
	    (Sc.Entity.id e_new_user);
	];
      ])
