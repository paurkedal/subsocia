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

module type S = Subsocia_intf.RW

val schema_prefix : string ref
val format_query : string -> Caqti_query.query
val fetch_grade : float

module type CONNECTION_POOL = sig
  val pool : (module Caqti_lwt.CONNECTION) Caqti_lwt.Pool.t
end

module type ENTITY_PLUGIN_FUNCTOR =
  functor (Pool : CONNECTION_POOL) -> Subsocia_intf.ENTITY_PLUGIN

val register_entity_plugin : string -> (module ENTITY_PLUGIN_FUNCTOR) -> unit

val connect : Uri.t -> (module S)
