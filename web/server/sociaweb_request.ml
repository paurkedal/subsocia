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
open Sociaweb_auth
open Sociaweb_connection
open Unprime_list
open Unprime_option

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

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
