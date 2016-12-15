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

open Eliom_client
open Panograph_i18n
open Sociaweb_auth
open Subsocia_connection
open Unprime_list
open Unprime_option

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

let http_redirect ~service get =
  let uri = Eliom_uri.make_string_uri ~absolute:true ~service get in
  let hdrs = Http_headers.empty |> Http_headers.(add location) uri in
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (302, None, Some hdrs))

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
  cri_operator : Entity.t;
  cri_langs : lang list;
}

let authenticate_cri () =
  let%lwt cri_operator = authenticate () in
  let cri_langs = match request_info_langs () with
                  | [] -> [Lang.of_string "en"]
                  | langs -> langs in
  Lwt.return {cri_operator; cri_langs}

(* Utility Functions *)

let auth_sf json f =
  let f' tup =
    let%lwt operator = authenticate () in
    f ~operator tup in
  server_function json f'
