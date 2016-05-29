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

[%%shared
open Eliom_service
open Eliom_parameter

let entities_service =
  App.service ~path:["entities"]
              ~get_params:(suffix (opt (int32 "entity_id"))) ()

let entities_self_service =
  App.service ~path:["entities"; "self"] ~get_params:unit ()

let registration_form_service =
  Http.service ~path:["registration"] ~get_params:unit ()

let registration_post_service =
  Http.post_service ~fallback:registration_form_service
    ~post_params:(string "first_name" **
                  string "last_name" **
                  string "email") ()
]
