(* Copyright (C) 2014--2015  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signatures for the core API. *)

open Panograph_i18n
open Subsocia_common

module type ATTRIBUTE_TYPE = sig
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

module type ATTRIBUTE = sig
  module Attribute_type : ATTRIBUTE_TYPE
  type t0 = Ex : 'a Attribute_type.t1 * 'a -> t0
end

module type ENTITY_TYPE = sig
  module Attribute_type : ATTRIBUTE_TYPE

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
  val entity_name_tmpl : t -> string Lwt.t
  val set_entity_name_tmpl : t -> string -> unit Lwt.t

  val inclusion : t -> t -> (Multiplicity.t * Multiplicity.t) option Lwt.t
  val dsub : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
  val dsuper : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
  val inclusion_dump :
    unit -> (t * t * Multiplicity.t * Multiplicity.t) list Lwt.t
  val inclusion_allow : Multiplicity.t -> Multiplicity.t -> t -> t ->
			unit Lwt.t
  val inclusion_disallow : t -> t -> unit Lwt.t

  val attribution_mult : t -> t -> 'a Attribute_type.t1 ->
			 Multiplicity.t option Lwt.t
  val attribution : t -> t -> Multiplicity.t Attribute_type.Map.t Lwt.t
  val attribution_dump :
    unit -> (t * t * Attribute_type.t0 * Multiplicity.t) list Lwt.t
  val attribution_allow : t -> t -> Attribute_type.t0 -> Multiplicity.t ->
			  unit Lwt.t
  val attribution_disallow : t -> t -> Attribute_type.t0 -> unit Lwt.t
end

module type ENTITY = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Entity_type : ENTITY_TYPE with module Attribute_type := Attribute_type

  type t

  module Set : SET with type elt = t
  module Map : MAP with type key = t

  val create : ?access: t -> Entity_type.t -> t Lwt.t
  val modify : ?access: t -> t -> unit Lwt.t
  val delete : t -> unit Lwt.t

  val compare : t -> t -> int

  val of_id : int32 -> t Lwt.t
  val id : t -> int32

  val type_ : t -> Entity_type.t Lwt.t
  val rank : t -> int Lwt.t
  val access : t -> t Lwt.t

  val type_members : Entity_type.t -> Set.t Lwt.t
  val top : t Lwt.t
  val minimums : unit -> Set.t Lwt.t
  val dsub : t -> Set.t Lwt.t
  val dsuper : t -> Set.t Lwt.t
  val precedes : t -> t -> bool Lwt.t

  val getattr : t -> t -> 'a Attribute_type.t1 -> 'a Values.t Lwt.t
  val setattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
  val addattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
  val delattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t

  val asub : t -> 'a Attribute_type.t1 -> 'a -> Set.t Lwt.t
  (** [asub e at v] are the attribution sub-entities of [e] along [at] gaining
      the value [v]. *)

  val asuper : t -> 'a Attribute_type.t1 -> 'a -> Set.t Lwt.t
  (** [asuper e at v] are the attribution super-entities of [e] along [at]
      loosing the value [v]. *)

  val apsub : t -> 'a Attribute_type.t1 -> 'a Values.t Map.t Lwt.t
  (** [apsub e at] is a map of [at]-values indexed by attribution sub-entities
      of [e] which gain those values along [at].  The function name is short
      for "attribute presence sub-entities". *)

  val apsuper : t -> 'a Attribute_type.t1 -> 'a Values.t Map.t Lwt.t
  (** [apsuper e at] is a map of [at]-values indexed by attribution
      super-entities of [e] which loose those values along [at]. The function
      name is short for "attribute presence super-entities". *)

  val constrain : t -> t -> unit Lwt.t
  (** [constrain e e'] forces an inclusion of [e] in [e'].
      @raise Invalid_argument if [e'] is included in [e]. *)

  val unconstrain : t -> t -> unit Lwt.t
  (** [unconstrain e e'] relaxes an inclusion of [e] in [e']. Only a direct
      inclusion is relaxed. [e] may still be included in [e'] though a set of
      intermediate inclusions after this call. *)
end

module type S = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Attribute : ATTRIBUTE with module Attribute_type := Attribute_type
  module Entity_type : ENTITY_TYPE with module Attribute_type := Attribute_type
  module Entity : ENTITY with module Attribute_type := Attribute_type
			  and module Entity_type := Entity_type
end
