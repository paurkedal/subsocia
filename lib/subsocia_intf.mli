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

module type S = sig

  module Attribute_key : sig
    type 'a t
    type any_t = Any_t : 'a t -> any_t

    module Set : Set.S with type elt = any_t
    module Map : Map.S with type key = any_t

    val of_name : string -> any_t Lwt.t
    val name : any_t -> string
    val value_type : any_t -> any_value_type
  end

  module Entity_type : sig
    type t

    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t

    val compare : t -> t -> int
    val of_name : string -> t option Lwt.t
    val name : t -> string Lwt.t
    val display_name : langs: lang list -> ?pl: bool -> t -> string Lwt.t
    val inclusion_preds : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
    val inclusion_succs : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
    val attribution : t -> t -> Multiplicity.t Attribute_key.Map.t Lwt.t
    val attribution_preds : t -> Multiplicity.t Attribute_key.Map.t Map.t Lwt.t
    val attribution_succs : t -> Multiplicity.t Attribute_key.Map.t Map.t Lwt.t
  end

  module Entity : sig
    type t

    module Set : Set.S with type elt = t
    module Map : Map.S with type key = t

    val create : entity_type: Entity_type.t -> viewer: t -> admin: t ->
		 unit -> t Lwt.t

    val compare : t -> t -> int

    val type_ : t -> Entity_type.t Lwt.t
    val viewer : t -> t Lwt.t
    val admin : t -> t Lwt.t

    val minimums : unit -> Set.t Lwt.t
    val maximums : unit -> Set.t Lwt.t
    val preds : t -> Set.t Lwt.t
    val succs : t -> Set.t Lwt.t
    val precedes : t -> t -> bool Lwt.t

    val fetch_attribute : t -> t -> 'a Attribute_key.t -> 'a list Lwt.t
    val store_attribute : t -> t -> 'a Attribute_key.t -> 'a list -> unit Lwt.t
    val display_name : langs: lang list -> t -> string Lwt.t

    val constrain : t -> t -> unit Lwt.t
    val unconstrain : t -> t -> unit Lwt.t
  end
end
