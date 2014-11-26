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

open Panograph_i18n
open Subsocia_common

module type ENTITY_PLUGIN = sig
  val attributes : attribute_info list

  type id = int32
  type t

  val fetch : id -> t Lwt.t
  val fetch_display_name : t -> langs: lang list -> string Lwt.t
  val fetch_attribute : t -> 'a attribute_key -> 'a Lwt.t
  val store_attribute : t -> 'a attribute_key -> 'a -> unit Lwt.t
end

module type ENTITY_TYPE = sig
  type id = int32
  type t

  val fetch : id -> t Lwt.t
  val get_id : t -> id
  val get_name : t -> string
  val get_display_name : langs: lang list -> ?pl: bool -> t -> string
  val get_preds : t -> (id * Multiplicity.t) list
  val get_succs : t -> (id * Multiplicity.t) list

  val get_plugin : t -> (module ENTITY_PLUGIN)
end

module type ENTITY_RO = sig
  type entity_type_id
  type entity_type

  type id = int32
  type t

  val get_id : t -> id
  val get_type_id : t -> entity_type_id
  val get_viewer_id : t -> id
  val get_admin_id : t -> id

  val fetch : id -> t Lwt.t
  val fetch_type : t -> entity_type Lwt.t
  val fetch_viewer : t -> t Lwt.t
  val fetch_admin : t -> t Lwt.t

  val fetch_min : unit -> t list Lwt.t
  val fetch_max : unit -> t list Lwt.t
  val fetch_preds : t -> t list Lwt.t
  val fetch_succs : t -> t list Lwt.t
  val check_preceq : t -> t -> bool Lwt.t

  val fetch_display_name : t -> langs: lang list -> string Lwt.t
  val fetch_attribute : t -> 'a attribute_key -> 'a Lwt.t
end

module type ENTITY_RW = sig

  include ENTITY_RO

  val create : entity_type: entity_type -> viewer: t -> admin: t ->
	       unit -> t Lwt.t

  val constrain : t -> t -> unit Lwt.t
  val unconstrain : t -> t -> unit Lwt.t

  val store_attribute : t -> 'a attribute_key -> 'a -> unit Lwt.t
end

module type RO = sig
  module Entity_type : ENTITY_TYPE
  module Entity : ENTITY_RO with type entity_type_id := Entity_type.id
			     and type entity_type := Entity_type.t
end

module type RW = sig
  module Entity_type : ENTITY_TYPE
  module Entity : ENTITY_RW with type entity_type_id := Entity_type.id
			     and type entity_type := Entity_type.t
end
