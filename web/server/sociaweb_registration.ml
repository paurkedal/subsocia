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

open Eliom_content
open Eliom_content.Html5
open Sociaweb_auth
open Sociaweb_request
open Sociaweb_services
open Subsocia_connection

let registration_form (first_name, (last_name, email)) =
  [F.table ~a:[F.a_class ["assoc"]] [
    F.tr [
      F.th [F.pcdata "First name:"];
      F.td [F.Form.input ~input_type:`Text ~name:first_name F.Form.string];
    ];
    F.tr [
      F.th [F.pcdata "Last name:"];
      F.td [F.Form.input ~input_type:`Text ~name:last_name F.Form.string];
    ];
    F.tr [
      F.th [F.pcdata "Email:"];
      F.td [F.Form.input ~input_type:`Email ~name:email F.Form.string];
    ];
    F.tr [
      F.td [];
      F.td [F.Form.input ~input_type:`Submit ~value:"Register" F.Form.string];
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
        F.Form.post_form ~service:registration_post_service
                         registration_form ();
      ])

let () =
  Eliom_registration.Html5.register ~service:registration_post_service
    @@ fun () (first_name, (last_name, email)) ->
  begin match%lwt get_operator_opt () with
  | None -> Lwt.return_unit
  | Some _ -> http_error 400 "Already registered."
  end >>
  let%lwt auth = get_authenticalia () in
  let%lwt at_unique_name = Const.at_unique_name in
  let%lwt at_first_name = Const.at_first_name in
  let%lwt at_last_name = Const.at_last_name in
  let%lwt at_email = Const.at_email in
  let%lwt e_root = Entity.root in
  let%lwt et_person = Const.et_person in
  let%lwt e_new_user = Entity.create et_person in
  let%lwt e_new_user_id = Entity.soid e_new_user in
  let%lwt e_new_users = Const.e_new_users in
  Entity.force_dsub e_new_user e_new_users >>
  Entity.set_value at_first_name first_name e_root e_new_user >>
  Entity.set_value at_last_name  last_name  e_root e_new_user >>
  Entity.set_value at_email      email      e_root e_new_user >>
  set_authenticalia e_new_user auth >>
  Lwt.return @@
    Eliom_tools.F.html
      ~title:"Welcome"
      ~css:[["css"; "subsocia.css"]]
      (F.body [
        F.h1 [F.pcdata "Welcome, "; F.pcdata first_name];
        F.p [
          F.a ~service:entities_service
            [F.pcdata "Your registration is complete."]
            (Some e_new_user_id);
        ];
      ])
