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

  val of_id : int32 -> ex Lwt.t
  val of_name : string -> ex option Lwt.t
  val id : 'a t -> int32
  val name : 'a t -> string Lwt.t
  val value_type : 'a t -> 'a Type.t
  val value_mult : 'a t -> Multiplicity.t
  val create : ?mult: Multiplicity.t -> 'a Type.t -> string -> 'a t Lwt.t
  val delete : 'a t -> unit Lwt.t

  (**/**)
  type 'a t1 = 'a t		[@@ocaml.deprecated "Renamed to t"]
  type t0 = ex			[@@ocaml.deprecated "Renamed to ex"]
  val type0 : ex -> Type.ex	[@@ocaml.deprecated "Use value_type"]
  val type1 : 'a t -> 'a Type.t	[@@ocaml.deprecated "Renamed to value_type"]
  val name' : 'a t -> string Lwt.t	[@@ocaml.deprecated "Alias for name."]
  val id' : 'a t -> int32		[@@ocaml.deprecetad "Alias for id."]
  val create' : ?mult: Multiplicity.t -> 'a Type.t -> string -> 'a t Lwt.t
					[@@ocaml.deprecated "Alias for create."]
  val delete' : 'a t -> unit Lwt.t	[@@ocaml.deprecated "Alias for delete."]
end

module type ATTRIBUTE_UNIQUENESS = sig
  module Attribute_type : ATTRIBUTE_TYPE
  type t

  module Set : SET with type elt = t
  module Map : MAP with type key = t

  exception Not_unique of Set.t

  val of_id : int32 -> t Lwt.t
  val id : t -> int32
  val force : Attribute_type.Set.t -> t Lwt.t
  val relax : t -> unit Lwt.t
  val find : Attribute_type.Set.t -> t option Lwt.t
  val all : unit -> Set.t Lwt.t
  val affecting : 'a Attribute_type.t -> Set.t Lwt.t
  val affected : t -> Attribute_type.Set.t Lwt.t
end

module type ATTRIBUTE = sig
  module Attribute_type : ATTRIBUTE_TYPE

  type ex = Ex : 'a Attribute_type.t * 'a -> ex

  type predicate =
    | Inter : predicate list -> predicate
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

  val can_attribute : 'a Attribute_type.t -> t -> t -> bool Lwt.t
  (** [can_attribute at et et'] indicates whether at attribute of type [at] is
      allowed from [et] to [et']. *)

  val allowed_attributes : t -> t -> Attribute_type.Set.t Lwt.t
  (** [allowed_attributes et et'] is the set of attribute types allowed from
      [et] to [et']. *)

  val allowed_attributions : unit -> (Attribute_type.ex * t * t) list Lwt.t

  val allow_attribution : 'a Attribute_type.t -> t -> t -> unit Lwt.t
  (** [allow_attribution at et et'] decleares that attributes of type [at] are
      allowed from entities of type [et] to entities of type [et']. *)

  val disallow_attribution : 'a Attribute_type.t -> t -> t -> unit Lwt.t
  (** [disallow_attribution at et et'] declares that attributes of type [at]
      are no longer allowed from entities of type [et] to entities of type
      [et'].  Current attributions of this type will remain until cleaned up,
      but algorithms are free to disregard them. *)

  (**/**)
  val can_asub : t -> t -> 'a Attribute_type.t -> Multiplicity.t option Lwt.t
  [@@ocaml.deprecated "Use can_attribute, but note new argument order."]
  val can_asub_byattr : t -> t -> Multiplicity.t Attribute_type.Map.t Lwt.t
  [@@ocaml.deprecated "Use allowed_attributes, but note new argument order."]
  val asub_elements :
    unit -> (t * t * Attribute_type.ex * Multiplicity.t) list Lwt.t
  [@@ocaml.deprecated "Use allowed_attributions, but note new argument order."]
  val allow_asub : t -> t -> Attribute_type.ex -> Multiplicity.t -> unit Lwt.t
  [@@ocaml.deprecated "Use allow_attribution, but note new argument order."]
  val disallow_asub : t -> t -> Attribute_type.ex -> unit Lwt.t
  [@@ocaml.deprecated "Use disallow_attribution, but note new argument order."]
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

  val is_dsub : t -> t -> bool Lwt.t
  (** [is_dsub e e'] holds iff [e] is a direct subentity of [e']. *)

  val is_sub : t -> t -> bool Lwt.t
  (** [is_sub e e'] holds iff [e] is a subentity of [e']. *)

  val get_values : 'a Attribute_type.t -> t -> t -> 'a Values.t Lwt.t
  (** [get_values at e e'] are the values of attributions of type [at]
      directed from [e] to [e']. *)

  val add_values : 'a Attribute_type.t -> 'a Values.t -> t -> t -> unit Lwt.t
  (** [add_values at xs e e'] adds attributions of type [at] with values [xs]
      from [e] to [e']. *)

  val remove_values : 'a Attribute_type.t -> 'a Values.t -> t -> t -> unit Lwt.t
  (** [remove_values at e e'] removes values [xs] from attributions of type
      [at] directed from [e] to [e']. *)

  val set_values : 'a Attribute_type.t -> 'a Values.t -> t -> t -> unit Lwt.t
  (** [set_values at xs e e'] replaces values of attributions from [e] to
      [e'] of type [at] with [xs]. *)

  val image1 : Attribute.predicate -> t -> Set.t Lwt.t
  (** [asub p e] are the attribution subentities of [e] along attributes for
      which [p] holds. *)

  val preimage1 : Attribute.predicate -> t -> Set.t Lwt.t
  (** [asub p e] are the attribution superentities of [e] along attributes for
      which [p] holds. *)

  val image1_eq : 'a Attribute_type.t -> 'a -> t -> Set.t Lwt.t
  (** [image1_eq at v e] are the attribution subentities of [e] along [at]
      gaining the value [v]. *)

  val preimage1_eq : 'a Attribute_type.t -> 'a -> t -> Set.t Lwt.t
  (** [preimage1_eq at v e] are the attribution superentities of [e] along
      [at] loosing the value [v]. *)

  val image1_fts : ?entity_type: Entity_type.t -> ?super: t ->
		   ?cutoff: float -> ?limit: int ->
		   Subsocia_fts.t -> t -> (t * float) list Lwt.t
  (** [image1_fts q e] are relevance-weighted subentities along text
      attributes matching a full-text search for [q], ordered by relevance.
      @param entity_type Only include entities of this type if specified.
      @param super Restrict the result to entities strictly below [super].
      @param limit The maximum number of entities to return. Default no limit.
      @param cutoff Results with [rank <= cutoff] are excluded. Default 0.0. *)

  val preimage1_fts : ?entity_type: Entity_type.t -> ?super: t ->
		      ?cutoff: float -> ?limit: int ->
		      Subsocia_fts.t -> t -> (t * float) list Lwt.t
  (** [preimage1_fts q e] are relevance-weighted superentities along text
      attributes matching a full-text search for [q], ordered by relevance.
      @param entity_type Only include entities of this type if specified.
      @param super Restrict the result to entities strictly below [super].
      @param limit The maximum number of entities to return. Default no limit.
      @param cutoff Results with [rank <= cutoff] are excluded. Default 0.0. *)

  val mapping1 : 'a Attribute_type.t -> t -> 'a Values.t Map.t Lwt.t
  (** [mapping1 at e] is a map of [at]-values indexed by attribution
      subentities of [e] which gain those values along [at]. *)

  val premapping1 : 'a Attribute_type.t -> t -> 'a Values.t Map.t Lwt.t
  (** [premapping1 at e] is a map of [at]-values indexed by attribution
      superentities of [e] which loose those values along [at]. *)

  (**/**)
  val getattr : t -> t -> 'a Attribute_type.t -> 'a Values.t Lwt.t
  [@@ocaml.deprecated "Use get_values, but note new argument order."]
  val setattr : t -> t -> 'a Attribute_type.t -> 'a list -> unit Lwt.t
  [@@ocaml.deprecated "Use set_values, but note new argument order."]
  val addattr : t -> t -> 'a Attribute_type.t -> 'a list -> unit Lwt.t
  [@@ocaml.deprecated "Use add_values, but note new argument order."]
  val delattr : t -> t -> 'a Attribute_type.t -> 'a list -> unit Lwt.t
  [@@ocaml.deprecated "Use remove_values, but note new argument order."]
  val asub : t -> Attribute.predicate -> Set.t Lwt.t
  [@@ocaml.deprecated "Use image1."]
  val asuper : t -> Attribute.predicate -> Set.t Lwt.t
  [@@ocaml.deprecated "Use preimage1."]
  val asub_conj : t -> Attribute.predicate list -> Set.t Lwt.t
  [@@ocaml.deprecated "Use image1 with Attribute.Inter."]
  val asuper_conj : t -> Attribute.predicate list -> Set.t Lwt.t
  [@@ocaml.deprecated "Use preimage1 with Attribute.Inter."]
  val asub_eq : t -> 'a Attribute_type.t -> 'a -> Set.t Lwt.t
  [@@ocaml.deprecated "Use image1_eq."]
  val asuper_eq : t -> 'a Attribute_type.t -> 'a -> Set.t Lwt.t
  [@@ocaml.deprecated "Use preimage1_eq."]
  val asub_fts : ?entity_type: Entity_type.t -> ?super: t ->
		 ?cutoff: float -> ?limit: int ->
		 t -> Subsocia_fts.t -> (t * float) list Lwt.t
  [@@ocaml.deprecated "Use image1_fts."]
  val asuper_fts : ?entity_type: Entity_type.t -> ?super: t ->
		   ?cutoff: float -> ?limit: int ->
		   t -> Subsocia_fts.t -> (t * float) list Lwt.t
  [@@ocaml.deprecated "Use preimage1_fts."]
  val asub_get : t -> 'a Attribute_type.t -> 'a Values.t Map.t Lwt.t
  [@@ocaml.deprecated "Use mapping1_fts."]
  val asuper_get : t -> 'a Attribute_type.t -> 'a Values.t Map.t Lwt.t
  [@@ocaml.deprecated "Use premapping1_fts."]
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
