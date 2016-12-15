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

open Eliom_service
open Eliom_parameter

let entities_service =
  create ~path:(Path ["entities"])
         ~meth:(Get (suffix (opt (int32 "entity_id")))) ()

let entities_self_service =
  create ~path:(Path ["entities"; "self"]) ~meth:(Get unit) ()

let registration_form_service =
  create ~path:(Path ["registration"]) ~meth:(Get unit) ()

let registration_post_service =
  let post = string "first_name" ** string "last_name" ** string "email" in
  create_attached_post ~fallback:registration_form_service
                       ~post_params:post ()

let%client entities_service = ~%entities_service
