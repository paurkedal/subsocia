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

open Eliom_pervasives

let subsocia_uri = Uri.of_string "postgresql:/"
module Sc = (val Subsocia_direct.connect subsocia_uri)
module Config = struct
  let display_name_attributes = Subsocia_config.display_name#get
end
module Scd = Subsocia_derived.Make (Config) (Sc)

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

let get_auth_http_header () =
  let h = Subsocia_config.Web.auth_http_header#get in
  let ri = Eliom_request_info.get_ri () in
  let frame = Ocsigen_extensions.Ocsigen_request_info.http_frame ri in
  Ocsigen_headers.find h frame

let e_auth_group = Scd.Entity.of_unique_name Subsocia_config.Web.auth_group#get

module Log_auth = struct
  let section = Lwt_log.Section.make "subsocia.auth"
  let debug_f fmt = Lwt_log.debug_f ~section fmt
end

let auth_identity () =
  try Lwt.return (get_auth_http_header ())
  with Not_found -> http_error 401 "Not authenticated."

let auth_entity () =
  lwt user = auth_identity () in
  Log_auth.debug_f "HTTP authenticated user is %s." user >>
  lwt e_auth_group = e_auth_group in
  lwt at_unique_name = Scd.Const.at_unique_name in
  lwt s = Sc.Entity.apreds e_auth_group at_unique_name user in
  match Sc.Entity.Set.cardinal s with
  | 1 -> Lwt.return (Sc.Entity.Set.min_elt s)
  | 0 -> http_error 403 "Not registered."
  | _ -> http_error 500 "Duplicate registration."
