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
open Subsocia_common
open Printf
open Unprime_list

let allowed_attributes = lazy
  (List.fold String_set.add Subsocia_config.Web.restapi_allowed_attributes#get
	     String_set.empty)

let bprint_json_value buf = function
  | `Bool b -> Buffer.add_string buf (if b then "true" else "false")
  | `Int i -> Buffer.add_string buf (string_of_int i)
  | `String s ->
    Buffer.add_char buf '"';
    Buffer.add_string buf (String.escaped s); (* FIXME *)
    Buffer.add_char buf '"'

let _ =
  Eliom_registration.Any.register_service
    ~path:["restapi"; "authorize"]
    ~get_params:Eliom_parameter.(
	suffix_prod (string "authcid")
		    (set string "must" ** set string "may" ** set string "q"))
    @@ fun (authcid, (must, (may, query))) () ->
  Lwt_log.debug_f "Checking authcid %s against [%s] and optional [%s] groups"
		  authcid (String.concat ", " must) (String.concat ", " may) >>
  lwt top = Sc.Entity.top in
  let allowed_ans = Lazy.force allowed_attributes in
  lwt must_ok, may_ok, query_res =
    match_lwt entity_of_authcid authcid with
    | None -> Lwt.return (false, [], [])
    | Some user ->
      let is_member_of group =
	match_lwt Scd.Entity.of_unique_name group with
	| None -> Lwt.return false
	| Some group -> Sc.Entity.precedes user group in
      let get_attribute an =
	if not (String_set.contains an allowed_ans) then Lwt.return_none else
	match_lwt Sc.Attribute_type.of_name an with
	| None -> Lwt.return_none
	| Some (Sc.Attribute_type.Ex at) ->
	  let vt = Sc.Attribute_type.type1 at in
	  lwt vs = Sc.Entity.getattr user top at in
	  let vs = Values.elements vs in
	  Lwt.return (Some (an, List.map (Value.typed_to_poly vt) vs)) in
      lwt must_ok = Lwt_list.for_all_p is_member_of must in
      if must_ok then
	lwt may_res = Lwt_list.filter_p is_member_of may in
	lwt query_res = Pwt_list.fmap_p get_attribute query in
	Lwt.return (true, may_res, query_res)
      else
	Lwt.return (false, [], []) in
  let buf = Buffer.create 80 in
  bprintf buf "{must_ok: %b, may_ok: [" must_ok;
  List.iteri
    (fun i group ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_char buf '"';
      Buffer.add_string buf (String.escaped group);
      Buffer.add_char buf '"')
    may_ok;
  Buffer.add_char buf ']';
  List.iter
    (fun (an, vs) ->
      Buffer.add_string buf ", ";
      Buffer.add_string buf an;
      Buffer.add_string buf ": [";
      List.iteri
	(fun i v ->
	  if i > 0 then Buffer.add_string buf ", ";
	  bprint_json_value buf v)
	vs;
      Buffer.add_string buf "]")
    query_res;
  Buffer.add_string buf "}\n";
  Eliom_registration.String.send (Buffer.contents buf, "application/json")
