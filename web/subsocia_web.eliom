(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
}}

let main_handler () () =
  let open Html5.D in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"TODO: Write the Application"
      ~css:[["css"; "subsocia.css"]]
      (body [
	h1 [pcdata "TODO: Write the Application"];
      ])

module Main_app =
  Eliom_registration.App (struct let application_name = "main" end)
let main_service =
  Main_app.register_service ~path:[] ~get_params:Eliom_parameter.unit
			    main_handler
