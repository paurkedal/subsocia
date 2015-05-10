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

  val can_dsub : t -> t -> (Multiplicity.t * Multiplicity.t) option Lwt.t
  (** [can_dsub et et'] is the corresponding multiplicities if entities of
      type [et] can be direct subentities of entities of type [et']. *)

  val dsub : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
  (** [dsub et] fetches a map from types of candidate subentities of entities
      of [et] to the allowed multiplicity of the inclusion, with the subentity
      multiplicity first. *)

  val dsuper : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
  (** [dsuper et] fetches a map from types of candidate superentities of
      entities of [et] to the allowed multiplicity of the inclusion, with the
      subentity multiplicity first. *)

  val dsub_elements :
    unit -> (t * t * Multiplicity.t * Multiplicity.t) list Lwt.t
  (** [dsub_elements ()] fetches the complete subentitiy relation policy. *)

  val allow_dsub : Multiplicity.t -> Multiplicity.t -> t -> t -> unit Lwt.t
  (** After [allow_dsub m m' et et'], entities of type [et] can be direct
      subentities of entities of type [et']. *)

  val disallow_dsub : t -> t -> unit Lwt.t
  (** After [disallow_dsub et et'], entities of type [et] can no longer be
      direct subentities of entities of type [et']. *)

  val can_asub : t -> t -> 'a Attribute_type.t1 -> Multiplicity.t option Lwt.t
  val can_asub_byattr : t -> t -> Multiplicity.t Attribute_type.Map.t Lwt.t
  val asub_elements :
    unit -> (t * t * Attribute_type.t0 * Multiplicity.t) list Lwt.t
  val allow_asub : t -> t -> Attribute_type.t0 -> Multiplicity.t -> unit Lwt.t
  val disallow_asub : t -> t -> Attribute_type.t0 -> unit Lwt.t
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
  (** [dsub e] fetches the direct subentities of [e]. *)

  val dsuper : t -> Set.t Lwt.t
  (** [dsuper e] fetches the direct superentities of [e]. *)

  val is_sub : t -> t -> bool Lwt.t
  (** [is_sub e e'] holds iff [e] is a subentity of [e']. *)

  val getattr : t -> t -> 'a Attribute_type.t1 -> 'a Values.t Lwt.t
  (** [getattr e e' at] fetches attributes from [e] to [e'] of type [at]. *)

  val setattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
  (** [setattr e e' at xs] replaces attributes from [e] to [e'] of type [et]
      with [xs]. *)

  val addattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
  (** [addattr e e' at xs] adds attributes [xs] of type [et] from [e] to
      [e']. *)

  val delattr : t -> t -> 'a Attribute_type.t1 -> 'a list -> unit Lwt.t
  (** [delattr e e' at] removes attributes [xs] of type [et] from [e] to
      [e']. *)

  val asub_eq : t -> 'a Attribute_type.t1 -> 'a -> Set.t Lwt.t
  (** [asub_eq e at v] are the attribution subentities of [e] along [at]
      gaining the value [v]. *)

  val asuper_eq : t -> 'a Attribute_type.t1 -> 'a -> Set.t Lwt.t
  (** [asuper_eq e at v] are the attribution superentities of [e] along [at]
      loosing the value [v]. *)

  val asub_get : t -> 'a Attribute_type.t1 -> 'a Values.t Map.t Lwt.t
  (** [asub_get e at] is a map of [at]-values indexed by attribution
      subentities of [e] which gain those values along [at]. *)

  val asuper_get : t -> 'a Attribute_type.t1 -> 'a Values.t Map.t Lwt.t
  (** [asuper_get e at] is a map of [at]-values indexed by attribution
      superentities of [e] which loose those values along [at]. *)

  val force_dsub : t -> t -> unit Lwt.t
  (** [force_dsub e e'] forces an inclusion of [e] in [e'].
      @raise Invalid_argument if [e'] is included in [e]. *)

  val relax_dsub : t -> t -> unit Lwt.t
  (** [relax_dsub e e'] relaxes an inclusion of [e] in [e']. Only a direct
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
