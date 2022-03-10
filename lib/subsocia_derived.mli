(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Implementation of the derived API. *)

module Make (Base : Subsocia_intf.S) :
  Subsocia_derived_intf.S
    with type Attribute_type.soid = Base.Attribute_type.soid
     and type 'a Attribute_type.t = 'a Base.Attribute_type.t
     and type Attribute_type.any = Base.Attribute_type.any
     and module Attribute_type.Set = Base.Attribute_type.Set
     and module Attribute_type.Map = Base.Attribute_type.Map
     and type Attribute_uniqueness.soid = Base.Attribute_uniqueness.soid
     and type Attribute_uniqueness.t = Base.Attribute_uniqueness.t
     and module Attribute_uniqueness.Set = Base.Attribute_uniqueness.Set
     and module Attribute_uniqueness.Map = Base.Attribute_uniqueness.Map
     and type Relation.t = Base.Relation.t
     and type Entity_type.soid = Base.Entity_type.soid
     and type Entity_type.t = Base.Entity_type.t
     and module Entity_type.Set = Base.Entity_type.Set
     and module Entity_type.Map = Base.Entity_type.Map
     and type Entity.soid = Base.Entity.soid
     and type Entity.t = Base.Entity.t
     and module Entity.Set = Base.Entity.Set
     and module Entity.Map = Base.Entity.Map
