(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

module type S = sig
  include Subsocia_derived_intf.S_SOID with type soid := int32

  type entity_change =
    [ `Force_dsub of Entity.t * Entity.t
    | `Relax_dsub of Entity.t * Entity.t
    | `Change_values of Entity.t * Entity.t ]

  val on_entity_change : (entity_change -> unit) -> unit
end

val connect : Uri.t -> (module S)

include S
