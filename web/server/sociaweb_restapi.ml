(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix
open Sociaweb_request
open Subsocia_common
open Subsocia_connection
open Subsocia_selector
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

let make_authorize_response must_ok may_ok query_res =
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
  Buffer.contents buf, "application/json"

let selected_entity s =
  try%lwt Entity.select_opt (selector_of_string s)
  with Failure msg | Invalid_argument msg -> http_error 400 msg

let restapi_service =
  let open Eliom_service in
  let get = Eliom_parameter.(
    string "subject" ** set string "must" ** set string "may" **
    set string "q") in
  create ~path:(Path ["restapi"; "authorize"]) ~meth:(Get get) ()

let _ =
  Eliom_registration.Any.register ~service:restapi_service
    @@ fun (subject, (must, (may, query))) () ->
  Lwt_log.debug_f
    "Checking %s against [%s] and optional [%s] groups"
    subject (String.concat ", " must) (String.concat ", " may) >>= fun () ->
  let%lwt root = Entity.root in
  let allowed_ans = Lazy.force allowed_attributes in
  let%lwt must_ok, may_ok, query_res =
    match%lwt selected_entity subject with
    | None -> Lwt.return (false, [], [])
    | Some user ->
      let is_member_of group =
        match%lwt selected_entity group with
        | None -> Lwt.return false
        | Some group -> Entity.is_sub user group in
      let get_attribute an =
        if not (String_set.mem an allowed_ans) then Lwt.return_none else
        match%lwt Attribute_type.of_name an with
        | None -> Lwt.return_none
        | Some (Attribute_type.Ex at) ->
          let vt = Attribute_type.value_type at in
          let%lwt vs = Entity.get_values at root user in
          let vs = Values.elements vs in
          Lwt.return (Some (an, List.map (Value.to_json vt) vs)) in
      let%lwt must_ok = Lwt_list.for_all_p is_member_of must in
      if must_ok then
        let%lwt may_res = Lwt_list.filter_p is_member_of may in
        let%lwt query_res = Lwt_list.filter_map_p get_attribute query in
        Lwt.return (true, may_res, query_res)
      else
        Lwt.return (false, [], []) in
  let resp = make_authorize_response must_ok may_ok query_res in
  Eliom_registration.String.send (resp)
