(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Parser and evaluator of expressions to select entities. *)

open Subsocia_selector_types

val selector_of_string : string -> selector
val string_of_selector : selector -> string

val add_selector_of_selector : selector -> add_selector
val selector_of_add_selector : add_selector -> selector

val delete_selector_of_selector : selector -> delete_selector
val selector_of_delete_selector : delete_selector -> selector

module Selector_utils (C : Subsocia_intf.S) : sig

  val select_from :
    ?time: Ptime.t -> selector ->
    C.Entity.Set.t -> C.Entity.Set.t Lwt.t

  val select : ?time: Ptime.t -> selector -> C.Entity.Set.t Lwt.t

  val select_one : ?time: Ptime.t -> selector -> C.Entity.t Lwt.t

  val select_opt : ?time: Ptime.t -> selector -> C.Entity.t option Lwt.t

end
