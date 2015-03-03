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

include module type of Subsocia_selector_types

val selector_of_string : string -> selector
val string_of_selector : selector -> string

module Selector_utils (C : Subsocia_intf.S) : sig
  val denote_selector : selector -> C.Entity.Set.t -> C.Entity.Set.t Lwt.t
  val select_entities : selector -> C.Entity.Set.t Lwt.t
  val select_entity : selector -> C.Entity.t Lwt.t
  val select_entity_opt : selector -> C.Entity.t option Lwt.t
end
