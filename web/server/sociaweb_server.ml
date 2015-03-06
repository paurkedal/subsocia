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
open Subsocia_selector
open Unprime_list
open Unprime_option

let subsocia_uri = Uri.of_string Subsocia_config.database_uri#get
module Sc = (val Subsocia_direct.connect subsocia_uri)
module Scd = struct
  include Subsocia_derived.Make (Sc)
  include Subsocia_selector.Selector_utils (Sc)
end

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

(* Authentication *)

type authenticalia = {
  auth_method : string;
  auth_identity : string;
}

let authentication_hook = ref []
let updating_autoreg_hook = ref []
let oneshot_autoreg_hook = ref []

module Log_auth = struct
  let section = Lwt_log.Section.make "subsocia.auth"
  let debug_f fmt = Lwt_log.debug_f ~section fmt
end

let get_authenticalia_opt () =
  match_lwt Pwt_list.search_s (fun p -> p ()) !authentication_hook with
  | Some _ as r -> Lwt.return r
  | None ->
    let ri = Eliom_request_info.get_ri () in
    let frame = Ocsigen_extensions.Ocsigen_request_info.http_frame ri in
    let auth_method =
      match Subsocia_config.Web.hba_method_header#get with
      | None -> None
      | Some hn -> Option.found (fun () -> Ocsigen_headers.find hn frame) in
    let auth_method =
      match auth_method with
      | None -> Subsocia_config.Web.hba_method#get
      | Some _ as r -> r in
    let auth_identity =
      match Subsocia_config.Web.hba_identity_header#get with
      | None -> None
      | Some hn -> Option.found (fun () -> Ocsigen_headers.find hn frame) in
    match auth_method, auth_identity with
    | Some auth_method, Some auth_identity ->
      Lwt.return (Some {auth_method; auth_identity})
    | None, _ | _, None ->
      Lwt.return_none

let get_authenticalia () =
  match_lwt get_authenticalia_opt () with
  | Some r -> Lwt.return r
  | None -> http_error 401 "Not authenticated."

let auth_top =
  let en = Subsocia_config.Web.auth_top#get in
  match_lwt Scd.select_entity_opt (selector_of_string en) with
  | None -> Lwt.fail (Failure ("Missing configured auth group "^en^"."))
  | Some e -> Lwt.return e

let auth_method_group name =
  lwt ag = auth_top in
  Scd.Entity.of_unique_name ~super:ag name

let entity_of_authenticalia auth =
  match_lwt auth_method_group auth.auth_method with
  | None -> Lwt.return_none
  | Some amg ->
    lwt at_unique_name = Scd.Const.at_unique_name in
    lwt s = Sc.Entity.apreds amg at_unique_name auth.auth_identity in
    match Sc.Entity.Set.cardinal s with
    | 1 -> Lwt.return (Some (Sc.Entity.Set.min_elt s))
    | 0 -> Lwt.return_none
    | _ -> http_error 500 "Duplicate registration."

let set_authenticalia subject auth =
  match_lwt auth_method_group auth.auth_method with
  | None -> http_error 500 "Missing group for authentication method."
  | Some amg ->
    lwt at_unique_name = Scd.Const.at_unique_name in
    lwt auth_top = auth_method_group auth.auth_method in
    Sc.Entity.setattr subject amg at_unique_name [auth.auth_identity]

let autoreg_entity_of_authenticalia auth =
  match_lwt Pwt_list.search_s (fun p -> p auth) !updating_autoreg_hook with
  | Some _ as r -> Lwt.return r
  | None ->
    match_lwt entity_of_authenticalia auth with
    | Some _ as r -> Lwt.return r
    | None -> Pwt_list.search_s (fun p -> p auth) !oneshot_autoreg_hook

let get_operator_opt () =
  match_lwt get_authenticalia_opt () with
  | None ->
    Log_auth.debug_f "Not authenticated." >>
    Lwt.return_none
  | Some auth ->
    Log_auth.debug_f "Authenicated %s with %s."
		     auth.auth_identity auth.auth_method >>
    autoreg_entity_of_authenticalia auth

let get_operator () =
  lwt auth = get_authenticalia () in
  match_lwt autoreg_entity_of_authenticalia auth with
  | Some e -> Lwt.return e
  | None -> http_error 403 "Not registered."

(* Request Info *)

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

(* Utility Functions *)

let auth_sf json f =
  let f' tup =
    lwt operator = get_operator () in
    f ~operator tup in
  server_function json f'
