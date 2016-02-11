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

open Eliom_content.Html5

let _ =
  Eliom_registration.Html5.register_service
    ~path:["debug"; "http_headers"] ~get_params:Eliom_parameter.unit
    (fun () () ->
      let ri = Eliom_request_info.get_ri () in
      let frame = Ocsigen_extensions.Ocsigen_request_info.http_frame ri in
      let fh = frame.Ocsigen_http_frame.frame_header in
      let hdrs = Ocsigen_http_frame.Http_header.get_headers fh in
      let tr_of_header name vs =
        F.tr [F.td [F.pcdata (Http_headers.name_to_string name)];
              F.td (List.map (fun s -> F.div [F.pcdata s]) vs)] in
      Lwt.return @@ Eliom_tools.F.html ~title:"HTTP Headers" @@ F.body [
        F.table ~a:[F.a_class ["std"]]
          (List.rev
            (Http_headers.fold
              (fun name vs acc -> tr_of_header name vs :: acc)
              hdrs []))
      ])
