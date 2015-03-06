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

open Sociaweb_connection

type authenticalia = {
  auth_method : string;
  auth_identity : string;
}

val authentication_hook : (unit -> authenticalia option Lwt.t) list ref
val updating_autoreg_hook : (authenticalia -> Sc.Entity.t option Lwt.t) list ref
val oneshot_autoreg_hook : (authenticalia -> Sc.Entity.t option Lwt.t) list ref

val get_authenticalia_opt : unit -> authenticalia option Lwt.t

val get_authenticalia : unit -> authenticalia Lwt.t

val entity_of_authenticalia : authenticalia -> Sc.Entity.t option Lwt.t

val set_authenticalia : Sc.Entity.t -> authenticalia -> unit Lwt.t

val autoreg_entity_of_authenticalia : authenticalia -> Sc.Entity.t option Lwt.t

val get_operator_opt : unit -> Sc.Entity.t option Lwt.t

val get_operator : unit -> Sc.Entity.t Lwt.t
