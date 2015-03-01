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

open Sociaweb_server
open Printf

let _ =
  Eliom_registration.Any.register_service
    ~path:["restapi"; "authorize"]
    ~get_params:Eliom_parameter.(string "authzgroup" **
				 string "authcid")
    @@ fun (authzgroup, authcid) () ->
  Lwt_log.debug_f "Checking authcid %s against %s" authcid authzgroup >>
  lwt status =
    match_lwt entity_of_authcid authcid with
    | None -> Lwt.return 2
    | Some user ->
      match_lwt Scd.Entity.of_unique_name authzgroup with
      | None -> Lwt.return 3
      | Some group ->
	lwt ok = Sc.Entity.precedes user group in
	Lwt.return (if ok then 0 else 1) in
  Eliom_registration.String.send (sprintf "%d\n" status, "text/plain")
