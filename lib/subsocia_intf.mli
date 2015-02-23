(* Copyright (C) 2014--2015  Petter Urkedal <paurkedal@gmail.com>
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

  module Attribute_type : sig
    type 'a t1
    type t0 = Ex : 'a t1 -> t0

    module Set : SET with type elt = t0
    module Map : MAP with type key = t0

    val of_name : string -> t0 option Lwt.t
    val name : t0 -> string Lwt.t
    val of_id : int32 -> t0 Lwt.t
    val id : t0 -> int32
    val type0 : t0 -> Type.t0
    val type1 : 'a t1 -> 'a Type.t1
    val create : Type.t0 -> string -> t0 Lwt.t
    val delete : t0 -> unit Lwt.t
  end

  module Entity_type : sig
    type t

    module Set : SET with type elt = t
    module Map : MAP with type key = t

    val compare : t -> t -> int
    val of_name : string -> t option Lwt.t
    val name : t -> string Lwt.t
    val of_id : int32 -> t Lwt.t
    val id : t -> int32
    val display_name : langs: lang list -> ?pl: bool -> t -> string Lwt.t
    val create : string -> t Lwt.t
    val delete : t -> unit Lwt.t
    val all : unit -> Set.t Lwt.t

    val inclusion : t -> t -> (Multiplicity.t * Multiplicity.t) option Lwt.t
    val inclusion_preds : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
    val inclusion_succs : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
    val inclusion_allow : Multiplicity.t -> Multiplicity.t -> t -> t ->
			  unit Lwt.t
    val inclusion_disallow : t -> t -> unit Lwt.t

    val attribution_mult0 : t -> t -> Attribute_type.t0 ->
			    Multiplicity.t option Lwt.t
    val attribution_mult1 : t -> t -> 'a Attribute_type.t1 ->
			    Multiplicity.t option Lwt.t
    val attribution : t -> t -> Multiplicity.t Attribute_type.Map.t Lwt.t
    val attribution_allow : t -> t -> Attribute_type.t0 -> Multiplicity.t ->
			    unit Lwt.t
    val attribution_disallow : t -> t -> Attribute_type.t0 -> unit Lwt.t
  end

  module Entity : sig
    type t

    module Set : SET with type elt = t
    module Map : MAP with type key = t

    val create : viewer: t -> admin: t -> Entity_type.t -> t Lwt.t

    val compare : t -> t -> int

    val of_id : int32 -> t Lwt.t
    val id : t -> int32

    val type_ : t -> Entity_type.t Lwt.t
    val viewer : t -> t Lwt.t
    val admin : t -> t Lwt.t

    val type_members : Entity_type.t -> Set.t Lwt.t
    val minimums : unit -> Set.t Lwt.t
    val maximums : unit -> Set.t Lwt.t
    val preds : t -> Set.t Lwt.t
    val succs : t -> Set.t Lwt.t
    val precedes : t -> t -> bool Lwt.t

    val getattr : t -> t -> 'a Attribute_type.t1 -> 'a Values.t Lwt.t
    val setattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
    val addattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
    val delattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t

    val apreds : t -> 'a Attribute_type.t1 -> 'a -> Set.t Lwt.t
    val asuccs : t -> 'a Attribute_type.t1 -> 'a -> Set.t Lwt.t

    val constrain : t -> t -> unit Lwt.t
    val unconstrain : t -> t -> unit Lwt.t
  end
end
