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
  open Sociaweb_services
  open Unprime_list
}}

{server{
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
