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
open Panograph_i18n
open Unprime_list
open Unprime_option

let subsocia_uri = Uri.of_string Subsocia_config.database_uri#get
module Sc = (val Subsocia_direct.connect subsocia_uri)
module Scd = Subsocia_derived.Make (Sc)

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

let get_auth_http_header () =
  let h = Subsocia_config.Web.auth_http_header#get in
  let ri = Eliom_request_info.get_ri () in
  let frame = Ocsigen_extensions.Ocsigen_request_info.http_frame ri in
  Ocsigen_headers.find h frame

let e_auth_group =
  let en = Subsocia_config.Web.auth_group#get in
  match_lwt Scd.Entity.of_unique_name en with
  | None -> Lwt.fail (Failure ("Missing configured auth group "^en^"."))
  | Some e -> Lwt.return e

module Log_auth = struct
  let section = Lwt_log.Section.make "subsocia.auth"
  let debug_f fmt = Lwt_log.debug_f ~section fmt
end

let auth_identity () =
  try Lwt.return (get_auth_http_header ())
  with Not_found -> http_error 401 "Not authenticated."

let entity_of_authcid user =
  lwt e_auth_group = e_auth_group in
  lwt at_unique_name = Scd.Const.at_unique_name in
  lwt s = Sc.Entity.apreds e_auth_group at_unique_name user in
  match Sc.Entity.Set.cardinal s with
  | 1 -> Lwt.return (Some (Sc.Entity.Set.min_elt s))
  | 0 -> Lwt.return_none
  | _ -> http_error 500 "Duplicate registration."

let get_operator_opt () =
  lwt user = auth_identity () in
  Log_auth.debug_f "HTTP authenticated user is %s." user >>
  entity_of_authcid user

let get_operator () =
  match_lwt get_operator_opt () with
  | Some e -> Lwt.return e
  | None -> http_error 403 "Not registered."

let request_info_langs () =
  let compare_al (_, qA) (_, qB) =
    compare (Option.get_or 1.0 qB) (Option.get_or 1.0 qA) in
  let decode_al (s, _) =
    try Some (Lang.of_string s)
    with Invalid_argument _ -> None in
  let als = List.sort compare_al (Eliom_request_info.get_accept_language ()) in
  List.fmap decode_al als

type custom_request_info = {
  cri_operator : Sc.Entity.t;
  cri_langs : lang list;
}

let get_custom_request_info () =
  lwt cri_operator = get_operator () in
  let cri_langs = match request_info_langs () with
		  | [] -> [Lang.of_string "en"]
		  | langs -> langs in
  Lwt.return {cri_operator; cri_langs}

let auth_sf json f =
  let f' tup =
    lwt operator = get_operator () in
    f ~operator tup in
  server_function json f'
