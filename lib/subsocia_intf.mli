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
  type 'a t
  type ex = Ex : 'a t -> ex

  module Set : SET with type elt = ex
  module Map : MAP with type key = ex

  val of_name : string -> ex option Lwt.t
  val name' : 'a t -> string Lwt.t
  val of_id : int32 -> ex Lwt.t
  val id' : 'a t -> int32
  val value_type : 'a t -> 'a Type.t
  val create' : 'a Type.t -> string -> 'a t Lwt.t
  val delete' : 'a t -> unit Lwt.t

  (**/**)
  type 'a t1 = 'a t		[@@ocaml.deprecated "Renamed to t"]
  type t0 = ex			[@@ocaml.deprecated "Renamed to ex"]
  val type0 : ex -> Type.ex	[@@ocaml.deprecated "Use value_type"]
  val type1 : 'a t -> 'a Type.t	[@@ocaml.deprecated "Renamed to value_type"]
  val name : ex -> string Lwt.t
  [@@ocaml.deprecated "The type of this function will change; \
		       the new version is available as name'"]
  val id : ex -> int32
  [@@ocaml.deprecated "The type of this function will change; \
		       the new version is available as id'"]
  val create : Type.ex -> string -> ex Lwt.t
  [@@ocaml.deprecated "The type of this function will change; \
		       the new version is available as create'"]
  val delete : ex -> unit Lwt.t
  [@@ocaml.deprecated "The type of this function will change; \
		       the new version is available as delete'"]
end

module type ATTRIBUTE_UNIQUENESS = sig
  module Attribute_type : ATTRIBUTE_TYPE
  type t

  module Set : SET with type elt = t
  module Map : MAP with type key = t

  val of_id : int32 -> t Lwt.t
  val id : t -> int32
  val force : Attribute_type.Set.t -> t Lwt.t
  val relax : t -> unit Lwt.t
  val find : Attribute_type.Set.t -> t option Lwt.t
  val affecting : 'a Attribute_type.t -> Set.t Lwt.t
  val affected : t -> Attribute_type.Set.t Lwt.t
end

module type ATTRIBUTE = sig
  module Attribute_type : ATTRIBUTE_TYPE

  type ex = Ex : 'a Attribute_type.t * 'a -> ex

  type predicate =
    | Present : 'a Attribute_type.t -> predicate
    | Eq : 'a Attribute_type.t * 'a -> predicate
    | In : 'a Attribute_type.t * 'a Values.t -> predicate
    | Leq : 'a Attribute_type.t * 'a -> predicate
    | Geq : 'a Attribute_type.t * 'a -> predicate
    | Between : 'a Attribute_type.t * 'a * 'a -> predicate
    | Search : string Attribute_type.t * Subsocia_re.t -> predicate
    | Search_fts : Subsocia_fts.t -> predicate

  (**/**)
  type t0 = ex [@@ocaml.deprecated "Renamed to ex"]
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
  (** [create name] creates an entity type named [name]. *)

  val delete : t -> unit Lwt.t
  (** [delete et] attempts to delete [et].  This will fail if there currently
      exists entities of this kind. *)

  val all : unit -> Set.t Lwt.t
  (** [all ()] is the set of all entity types. *)

  val entity_name_tmpl : t -> string Lwt.t
  (** [entity_name_tmpl et] is used to derive the display name for entities of
      type [et]. *)

  val set_entity_name_tmpl : t -> string -> unit Lwt.t
  (** [set_entity_name_tmpl et tmpl] sets the template returned by
      {!entity_name_tmpl}. *)

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
      direct subentities of entities of type [et'].  Current inclusions of
      this kind will remain until cleaned up, but algorithms are free to
      disregard them. *)

  val can_asub : t -> t -> 'a Attribute_type.t -> Multiplicity.t option Lwt.t
  (** [can_asub et et' at] is the allowed multiplicity of attributes of type
      [at] from [et] to [et'], or [None] if disallowed. *)

  val can_asub_byattr : t -> t -> Multiplicity.t Attribute_type.Map.t Lwt.t
  (** [can_asub_byattr et et'] is a map from allowed attributes from [et] to
      [et'] to the corresponding multiplicity. *)

  val asub_elements :
    unit -> (t * t * Attribute_type.ex * Multiplicity.t) list Lwt.t

  val allow_asub : t -> t -> Attribute_type.ex -> Multiplicity.t -> unit Lwt.t
  (** [allow_asub et et' at m] decleares that [at] is allowed with
      multiplicity [m] from [et] to [et']. *)

  val disallow_asub : t -> t -> Attribute_type.ex -> unit Lwt.t
  (** [disallow_asub et et' at] declares that [at] is no longer allowed from
      [et] to [et'].  Current attributions of this type will remain until
      cleaned up, but algorithms are free to disregard them. *)
end

module type ENTITY = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Attribute : ATTRIBUTE with module Attribute_type := Attribute_type
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

  val dsub : ?et: Entity_type.t -> t -> Set.t Lwt.t
  (** [dsub e] fetches the direct subentities of [e]. *)

  val dsuper : ?et: Entity_type.t -> t -> Set.t Lwt.t
  (** [dsuper e] fetches the direct superentities of [e]. *)

  val force_dsub : t -> t -> unit Lwt.t
  (** [force_dsub e e'] forces an inclusion of [e] in [e'].
      @raise Invalid_argument if [e'] is included in [e]. *)

  val relax_dsub : t -> t -> unit Lwt.t
  (** [relax_dsub e e'] relaxes an inclusion of [e] in [e']. Only a direct
      inclusion is relaxed. [e] may still be included in [e'] though a set of
      intermediate inclusions after this call. *)

  val is_sub : t -> t -> bool Lwt.t
  (** [is_sub e e'] holds iff [e] is a subentity of [e']. *)

  val getattr : t -> t -> 'a Attribute_type.t -> 'a Values.t Lwt.t
  (** [getattr e e' at] fetches attributes from [e] to [e'] of type [at]. *)

  val setattr : t -> t -> 'a Attribute_type.t -> 'a list -> unit Lwt.t
  (** [setattr e e' at xs] replaces attributes from [e] to [e'] of type [et]
      with [xs]. *)

  val addattr : t -> t -> 'a Attribute_type.t -> 'a list -> unit Lwt.t
  (** [addattr e e' at xs] adds attributes [xs] of type [et] from [e] to
      [e']. *)

  val delattr : t -> t -> 'a Attribute_type.t -> 'a list -> unit Lwt.t
  (** [delattr e e' at] removes attributes [xs] of type [et] from [e] to
      [e']. *)

  val asub : t -> Attribute.predicate -> Set.t Lwt.t
  (** [asub e p] are the attribution subentities of [e] along attributes for
      which [p] holds. *)

  val asuper : t -> Attribute.predicate -> Set.t Lwt.t
  (** [asub e p] are the attribution superentities of [e] along attributes for
      which [p] holds. *)

  val asub_conj : t -> Attribute.predicate list -> Set.t Lwt.t
  (** [asub_conj e ps] returns the set of elements linked from [e] by at least
      one attribute for each predicate [p] in [ps] fulfilling [p]. *)

  val asuper_conj : t -> Attribute.predicate list -> Set.t Lwt.t
  (** [asuper_conj e ps] returns the set of elements linked to [e] by at least
      one attribute for each predicate [p] in [ps] fulfilling [p]. *)

  val asub_eq : t -> 'a Attribute_type.t -> 'a -> Set.t Lwt.t
  (** [asub_eq e at v] are the attribution subentities of [e] along [at]
      gaining the value [v]. *)

  val asuper_eq : t -> 'a Attribute_type.t -> 'a -> Set.t Lwt.t
  (** [asuper_eq e at v] are the attribution superentities of [e] along [at]
      loosing the value [v]. *)

  val asub_fts : ?entity_type: Entity_type.t -> ?super: t ->
		 ?cutoff: float -> ?limit: int ->
		 t -> Subsocia_fts.t -> (t * float) list Lwt.t
  (** [asub_fts e q] are relevance-weighted subentities along text attributes
      matching a full-text search for [q], ordered by relevance.
      @param entity_type Only include entities of this type if specified.
      @param super Restrict the result to entities strictly below [super].
      @param limit The maximum number of entities to return. Default no limit.
      @param cutoff Results with [rank <= cutoff] are excluded. Default 0.0. *)

  val asuper_fts : ?entity_type: Entity_type.t -> ?super: t ->
		   ?cutoff: float -> ?limit: int ->
		   t -> Subsocia_fts.t -> (t * float) list Lwt.t
  (** [asuper_fts e q] are relevance-weighted superentities along text
      attributes matching a full-text search for [q], ordered by relevance.
      @param entity_type Only include entities of this type if specified.
      @param super Restrict the result to entities strictly below [super].
      @param limit The maximum number of entities to return. Default no limit.
      @param cutoff Results with [rank <= cutoff] are excluded. Default 0.0. *)

  val asub_get : t -> 'a Attribute_type.t -> 'a Values.t Map.t Lwt.t
  (** [asub_get e at] is a map of [at]-values indexed by attribution
      subentities of [e] which gain those values along [at]. *)

  val asuper_get : t -> 'a Attribute_type.t -> 'a Values.t Map.t Lwt.t
  (** [asuper_get e at] is a map of [at]-values indexed by attribution
      superentities of [e] which loose those values along [at]. *)
end

module type S = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Attribute_uniqueness : ATTRIBUTE_UNIQUENESS
    with module Attribute_type := Attribute_type
  module Attribute : ATTRIBUTE with module Attribute_type := Attribute_type
  module Entity_type : ENTITY_TYPE with module Attribute_type := Attribute_type
  module Entity : ENTITY with module Attribute_type := Attribute_type
			  and module Attribute := Attribute
			  and module Entity_type := Entity_type
end
