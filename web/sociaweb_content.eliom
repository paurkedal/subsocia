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

{shared{
  open Eliom_content
  open Unprime_list
}}

{server{
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
}}

{shared{

  let multicol ?(m = 4) ?(cls = []) items =
    let open Html5.F in
    let items = Array.of_list items in
    let l = Array.length items in
    let n = (l + m - 1) / m in
    let mktr i =
      let mktd j = td (if i + j * n < l then items.(i + j * n) else []) in
      tr (List.sample mktd m) in
    match List.sample mktr n with
    | [] -> div ~a:[a_class ("multicol" :: "empty" :: cls)] []
    | trs -> table ~a:[a_class ("multicol" :: cls)] trs

}}
