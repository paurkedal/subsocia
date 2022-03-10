(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

module type S = sig
  include Subsocia_intf.S_SOID with type soid := int32

  type entity_change =
    [ `Force_dsub of Entity.t * Entity.t
    | `Relax_dsub of Entity.t * Entity.t
    | `Change_values of Entity.t * Entity.t ]

  val on_entity_change : (entity_change -> unit) -> unit

  module type T = Subsocia_intf.S_SOID
    with type soid := int32
     and module Attribute_type = Attribute_type
     and module Attribute_uniqueness = Attribute_uniqueness
     and module Relation = Relation
     and module Entity_type = Entity_type
     and module Entity = Entity

  val transaction : ((module T) -> 'a Lwt.t) -> 'a Lwt.t

  val clear_caches : unit -> unit

  val db_uri : Uri.t

  val db_schema : string option
end
