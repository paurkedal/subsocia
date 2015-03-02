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
    ~get_params:Eliom_parameter.(
	suffix_prod (string "authcid") (set string "must" ** set string "may"))
    @@ fun (authcid, (must, may)) () ->
  Lwt_log.debug_f "Checking authcid %s against [%s] and optional [%s] groups"
		  authcid (String.concat ", " must) (String.concat ", " may) >>
  lwt must_ok, may_ok =
    match_lwt entity_of_authcid authcid with
    | None -> Lwt.return (false, [])
    | Some user ->
      let is_member_of group =
	match_lwt Scd.Entity.of_unique_name group with
	| None -> Lwt.return false
	| Some group -> Sc.Entity.precedes user group in
      lwt must_ok = Lwt_list.for_all_p is_member_of must in
      if must_ok then
	lwt may_res = Lwt_list.filter_p is_member_of may in
	Lwt.return (true, may_res)
      else
	Lwt.return (false, []) in
  let buf = Buffer.create 80 in
  bprintf buf "{must_ok: %b, may_ok: [" must_ok;
  List.iteri
    (fun i group ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_char buf '"';
      Buffer.add_string buf (String.escaped group);
      Buffer.add_char buf '"')
    may_ok;
  bprintf buf "]}\n";
  Eliom_registration.String.send (Buffer.contents buf, "application/json")
