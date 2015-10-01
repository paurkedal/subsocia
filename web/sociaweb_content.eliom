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

{shared{
  open Eliom_content.Html5
  open Lwt.Infix
  open Panui_completion
  open Sociaweb_services
  open Unprime_char
  open Unprime_list
  open Unprime_option
  open Unprime_string
}}

{server{
  open Sociaweb_auth
  open Sociaweb_request
  open Subsocia_connection

(*
  let client_node_lwt (m : [`Div] Html5.elt Lwt.t client_value) =
    let ph_el = Html5.D.div [] in
    ignore {unit{
      let ph_node = Html5.To_dom.of_div %ph_el in
      Lwt.async @@ fun () ->
      %m >|= fun el ->
      let n = Html5.To_dom.of_div el in
      Js.Opt.iter (ph_node##parentNode) (fun p -> Dom.replaceChild p n ph_node)
    }};
    ph_el
*)

  let entity_link ~langs ent =
    let id = Entity.id ent in
    lwt name = Entity.display_name ~langs ent in
    Lwt.return (F.a ~service:entity_service [F.pcdata name] id)
}}

{shared{

  let multicol_tds ?(m = 4) ?(cls = []) items =
    let items = Array.of_list items in
    let l = Array.length items in
    let n = (l + m - 1) / m in
    let mktr i =
      let mktd j = if i + j * n < l then items.(i + j * n) else F.td [] in
      F.tr (List.sample mktd m) in
    match List.sample mktr n with
    | [] -> F.div ~a:[F.a_class ("multicol" :: "empty" :: cls)] []
    | trs -> F.table ~a:[F.a_class ("multicol" :: cls)] trs

  let multicol ?m ?cls items =
    multicol_tds ?m ?cls (List.map (fun els -> F.td els) items)
}}

{server{
  let complete_helper entity_type_id super_id words_str =
    lwt operator = get_operator () in
    lwt super = Pwt_option.map_s Entity.of_id super_id in
    lwt can_search =
      match super with
      | None ->
	lwt top = Entity.top in
	Entity.can_search_below operator top
      | Some super ->
	Entity.can_search_below operator super in
    if not can_search then http_error 403 "Search not permitted." else
    lwt entity_type = Pwt_option.map_s Entity_type.of_id entity_type_id in
    let words = String.chop_consecutive Char.is_space words_str in
    match List.rev words with
    | [] -> Lwt.return []
    | word :: words ->
      let cutoff = Subsocia_config.Web.completion_cutoff#get in
      let limit = Subsocia_config.Web.completion_limit#get in
      let fts = Prime_buffer.with0 @@ fun buf ->
	Buffer.add_string buf word;
	Buffer.add_string buf ":*";
	List.iter (fun word -> Buffer.add_string buf " & ";
			       Buffer.add_string buf word) words in
      lwt root = Entity.top in
      Entity.image1_fts ?entity_type ?super ~cutoff ~limit fts root

  let complete (entity_type_id, super_id, words_str) =
    complete_helper entity_type_id super_id words_str
      >>= Lwt_list.map_s (fun (entity, _) -> Entity.display_name entity)

  let complete_sf =
    server_function Json.t<int32 option * int32 option * string> complete

  let completed (entity_type_id, super_id, str) =
    match_lwt
      complete_helper entity_type_id super_id str >>=
      Lwt_list.filter_s (fun (e, _) -> Entity.display_name e >|= (=) str)
    with
    | [(entity, _)] -> Lwt.return (Some (Entity.id entity))
    | _ -> Lwt.return None

  let completed_sf =
    server_function Json.t<int32 option * int32 option * string> completed
}}
{client{
  let entity_completion_input ?(entity_type_id : int32 option)
			      ?(super_id : int32 option) emit =
    let complete  s = %complete_sf (entity_type_id, super_id, s) in
    string_completion_input complete emit
}}
{server{
  let entity_completion_input ?entity_type ?super emit =
    let entity_type_id = Option.map Entity.id entity_type in
    let super_id = Option.map Entity.id super in
    let complete = {{fun s -> %complete_sf (%entity_type_id, %super_id, s)}} in
    string_completion_input complete emit
}}
