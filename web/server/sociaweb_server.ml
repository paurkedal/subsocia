(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

open Sociaweb_request
open Subsocia_connection

module Sociaweb_app =
  Eliom_registration.App (struct let application_name = "sociaweb_app" end)

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

let relax_dsub ~operator (lb_id, ub_id) =
  lwt ub = entity_for_edit ~operator ub_id in
  lwt lb = entity_for_view ~operator lb_id in
  lwt user_name = Entity.display_name ~langs:[] operator in
  Lwt_log.info_f "%s removes inclusion #%ld ⊆ #%ld" user_name lb_id ub_id >>
  Entity.relax_dsub lb ub

let force_dsub_sf = auth_sf Json.t<int32 * int32> force_dsub
let relax_dsub_sf = auth_sf Json.t<int32 * int32> relax_dsub

let () = Subsocia_plugin.load_web_plugins ()
