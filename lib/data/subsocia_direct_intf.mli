(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
  include Subsocia_intf.S

  val entity_changed : Entity.t -> [`Dsub | `Dsuper | `Asub | `Asuper] React.E.t

  module type T = Subsocia_intf.S
    with type 'a Attribute_type.t = 'a Attribute_type.t
     and type Attribute_type.ex = Attribute_type.ex
     and type Attribute_type.Set.t = Attribute_type.Set.t
     and type 'a Attribute_type.Map.t = 'a Attribute_type.Map.t
     and type Adjacency.t = Adjacency.t
     and type Attribute.ex = Attribute.ex
     and type Attribute.predicate = Adjacency.t
     and type Entity_type.t = Entity_type.t
     and type Entity_type.Set.t = Entity_type.Set.t
     and type 'a Entity_type.Map.t = 'a Entity_type.Map.t
     and type Entity.t = Entity.t
     and type Entity.Set.t = Entity.Set.t
     and type 'a Entity.Map.t = 'a Entity.Map.t

  val transaction : ((module T) -> unit Lwt.t) -> unit Lwt.t
end
