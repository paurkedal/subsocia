(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signatures for the core API. *)

open Subsocia_common

module type SOID = sig
  type soid

  val of_string : string -> soid
  (** For internal use. *)

  val to_string : soid -> string
  (** A textual representation for logging and debugging. *)

  val compare : soid -> soid -> int
  (** Strict order. *)
end

module type ATTRIBUTE_TYPE = sig
  type soid
  type 'a t
  type any = Any : 'a t -> any

  module Set : SET with type elt = any
  module Map : MAP with type key = any
  module Soid : SOID with type soid := soid

  val of_soid_exn : 'a Type.t -> soid -> 'a t Lwt.t
  (** [of_soid_exn vt soid] is the attribute type of ID [soid].
      @raise Subsocia_error.Exn
        if [soid] is invalid or if [vt] is not the type of values held by the
        attribute. *)

  val of_name_exn : 'a Type.t -> string -> 'a t Lwt.t
  (** [of_name_exn vt name] is the attribute type named [name].
      @raise Subsocia_error.Exn
        if there is no attribute type named [name] or if it does not hold values
        of type [vt]. *)

  val any_of_soid_exn : soid -> any Lwt.t
  (** [any_of_soid_exn name] is the attribute type named [name].
      @raise Subsocia_error.Exn if [soid] is not a valid attribute type ID. *)

  val any_of_name_exn : string -> any Lwt.t
  (** [any_of_name_exn name] is the attribute type named [name].
      @raise Subsocia_error.Exn if [name] does not name an attribute type. *)

  val coerce_any : 'a Type.t -> any -> 'a t option

  val soid : 'a t -> soid Lwt.t
  val name : 'a t -> string Lwt.t
  val value_type : 'a t -> 'a Type.t
  val value_mult : 'a t -> Multiplicity.t
  val create : ?mult: Multiplicity.t -> 'a Type.t -> string -> 'a t Lwt.t
  val delete : 'a t -> unit Lwt.t
  val all : unit -> Set.t Lwt.t
end

module type ATTRIBUTE_UNIQUENESS = sig
  module Attribute_type : ATTRIBUTE_TYPE
  type soid
  type t

  module Set : SET with type elt = t
  module Map : MAP with type key = t
  module Soid : SOID with type soid := soid

  exception Not_unique of Set.t

  val of_soid : soid -> t Lwt.t
  val soid : t -> soid Lwt.t
  val force : Attribute_type.Set.t -> t Lwt.t
  val relax : t -> unit Lwt.t
  val find : Attribute_type.Set.t -> t option Lwt.t
  val all : unit -> Set.t Lwt.t
  val affecting : 'a Attribute_type.t -> Set.t Lwt.t
  val affected : t -> Attribute_type.Set.t Lwt.t
end

module type RELATION = sig
  module Attribute_type : ATTRIBUTE_TYPE

  type t =
    | Inter : t list -> t
    | True : t
    | Present : 'a Attribute_type.t -> t
    | Eq : 'a Attribute_type.t * 'a -> t
    | In : 'a Attribute_type.t * 'a Values.t -> t
    | Leq : 'a Attribute_type.t * 'a -> t
    | Geq : 'a Attribute_type.t * 'a -> t
    | Between : 'a Attribute_type.t * 'a * 'a -> t
    | Search : string Attribute_type.t * Subsocia_re.t -> t
    | Search_fts : Subsocia_fts.t -> t
end

module type ENTITY_TYPE = sig
  module Attribute_type : ATTRIBUTE_TYPE

  type soid
  type t

  module Set : SET with type elt = t
  module Map : MAP with type key = t
  module Soid : SOID with type soid := soid

  val compare : t -> t -> int
  (** [compare] provides an arbitrary strict order of entity types. *)

  val of_name : string -> t option Lwt.t
  (** [of_name etn] is the entity type named [etn] if any. *)

  val of_name_exn : string -> t Lwt.t
  (** [of_name_exn etn] is the entity type named [etn].
      @raise Caqti_error.Exn if there is no such entity type. *)

  val name : t -> string Lwt.t
  (** [name et] is the name of [et]. *)

  val of_soid : soid -> t Lwt.t
  (** [of_id id] returns the entity of identified by [id], which is assumed to
      exist. *)

  val soid : t -> soid Lwt.t
  (** [id et] returns the numeric ID of [et]. *)

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

  val allowed_preimage : t -> Attribute_type.any list Map.t Lwt.t
  val allowed_image : t -> Attribute_type.any list Map.t Lwt.t

  val allowed_mappings : 'a Attribute_type.t -> (t * t) list Lwt.t
  (** [allowed_mappings at] is a list of domain and codomain of the valid
      attributions involving [at]. *)

  val allowed_attributions : unit -> (Attribute_type.any * t * t) list Lwt.t

  val allow_attribution : 'a Attribute_type.t -> t -> t -> unit Lwt.t
  (** [allow_attribution at et et'] decleares that attributes of type [at] are
      allowed from entities of type [et] to entities of type [et']. *)

  val disallow_attribution : 'a Attribute_type.t -> t -> t -> unit Lwt.t
  (** [disallow_attribution at et et'] declares that attributes of type [at]
      are no longer allowed from entities of type [et] to entities of type
      [et'].  Current attributions of this type will remain until cleaned up,
      but algorithms are free to disregard them. *)
end

module type ENTITY = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Relation : RELATION
    with module Attribute_type := Attribute_type
  module Entity_type : ENTITY_TYPE with module Attribute_type := Attribute_type

  type soid
  type t

  module Set : SET with type elt = t
  module Map : MAP with type key = t
  module Soid : SOID with type soid := soid

  val create : Entity_type.t -> t Lwt.t
  (** [create et] creates and returns an entity of the type [et], initially
      with no relation to other entities. *)

  val delete : t -> unit Lwt.t
  (** [delete e] deletes [e], which must have no remaining relation to other
      entities. *)

  val compare : t -> t -> int
  (** An arbitrary total order over entities. *)

  val of_soid : soid -> t Lwt.t
  (** [of_id id] is the entity identified by [id], which is assumed to exist. *)

  val soid : t -> soid Lwt.t
  (** [id e] is the numeric ID of [e]. *)

  val entity_type : t -> Entity_type.t Lwt.t
  (** [entity_type e] is the entity type of [e]. *)

  val rank : t -> int Lwt.t
  (** [rank e] is the rank of [e] in the inclusion graph.  An entity which is
      not included in other entities, has rank 0. *)

  val type_members : Entity_type.t -> Set.t Lwt.t

  val is_root : t -> bool Lwt.t
  (** [is_root e] is true iff [e] is the root element. *)

  val root : t Lwt.t
  [@@ocaml.deprecated "Use get_root."]

  val get_root : unit -> t Lwt.t
  (** [get_root ()] returns the root element. *)

  (** {2 Inclusion} *)

  val minimums : unit -> Set.t Lwt.t

  val dsub : ?time: Ptime.t -> ?et: Entity_type.t -> t -> Set.t Lwt.t
  (** [dsub e] fetches the direct subentities of [e].
      @param time The time at which to probe for inclusion, defaults to now. *)

  val dsuper : ?time: Ptime.t -> ?et: Entity_type.t -> t -> Set.t Lwt.t
  (** [dsuper e] fetches the direct superentities of [e].
      @param time The time at which to probe for inclusion, defaults to now. *)

  val sub : ?time: Ptime.t -> t -> Set.t Lwt.t
  (** The transitive closure of {!dsub}. *)

  val super : ?time: Ptime.t -> t -> Set.t Lwt.t
  (** The transitive closure of {!dsuper}. *)

  val dsub_history :
    ?since: Ptime.t -> ?until: Ptime.t -> ?et: Entity_type.t ->
    t -> (Ptime.t * Ptime.t option * t) list Lwt.t

  val dsuper_history :
    ?since: Ptime.t -> ?until: Ptime.t -> ?et: Entity_type.t ->
    t -> (Ptime.t * Ptime.t option * t) list Lwt.t

  val force_dsub : ?time: Ptime.t -> t -> t -> unit Lwt.t
  (** [force_dsub e e'] forces an inclusion of [e] in [e'].
      @raise Invalid_argument if [e'] is included in [e]. *)

  val relax_dsub : ?time: Ptime.t -> t -> t -> unit Lwt.t
  (** [relax_dsub e e'] relaxes an inclusion of [e] in [e']. Only a direct
      inclusion is relaxed. [e] may still be included in [e'] though a set of
      intermediate inclusions after this call. *)

  val is_dsub : ?time: Ptime.t -> t -> t -> bool Lwt.t
  (** [is_dsub e e'] holds iff [e] is a direct subentity of [e'].
      @param time The time at which to probe for inclusion, defaults to now. *)

  val is_sub : ?time: Ptime.t -> t -> t -> bool Lwt.t
  (** [is_sub e e'] holds iff [e] is a subentity of [e'].
      @param time The time at which to probe for inclusion, defaults to now. *)

  (** {2 Attribution} *)

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

  val image : Relation.t -> Set.t -> Set.t Lwt.t
  (** [image r ps] is the set of entities reachable by crossing an arrow which
      satisfies [r] starting from any entity in [ps]. *)

  val preimage : Relation.t -> Set.t -> Set.t Lwt.t
  (** [preimage r ps] is the set of entites reachable by crossing an arrow which
      satisifies [r] in reverse direction starting from any entity in [ps]. *)

  val image1 : Relation.t -> t -> Set.t Lwt.t
  (** [image1 p e] are the target entities of attributions with source [e] along
      attributes for which [p] holds. *)

  val preimage1 : Relation.t -> t -> Set.t Lwt.t
  (** [preimage1 p e] are the source entities of attributions with target [e]
      along which [p] holds. *)

  val image1_eq : 'a Attribute_type.t -> 'a -> t -> Set.t Lwt.t
  (** [image1_eq at v e] are the target entities of attributions with source [e]
      along which the attribute type [at] gains the value [v]. *)

  val preimage1_eq : 'a Attribute_type.t -> 'a -> t -> Set.t Lwt.t
  (** [preimage1_eq at v e] are the source entities of attributions with target
      [e] along which the attribute type [at] looses the value [v]. *)

  val image1_fts : ?entity_type: Entity_type.t -> ?super: t ->
                   ?cutoff: float -> ?limit: int ->
                   Subsocia_fts.t -> t -> (t * float) list Lwt.t
  (** [image1_fts q e] are relevance-weighted target entities of text
      attributions with source [e] matching a full-text search for [q], ordered
      by relevance.

      @param entity_type Only include entities of this type if specified.
      @param super Restrict the result to entities strictly below [super].
      @param limit The maximum number of entities to return. Default no limit.
      @param cutoff Results with [rank <= cutoff] are excluded. Default 0.0. *)

  val preimage1_fts : ?entity_type: Entity_type.t -> ?super: t ->
                      ?cutoff: float -> ?limit: int ->
                      Subsocia_fts.t -> t -> (t * float) list Lwt.t
  (** [preimage1_fts q e] are relevance-weighted source entities of text
      attributions with target [e] matching a full-text search for [q], ordered
      by relevance.

      @param entity_type Only include entities of this type if specified.
      @param super Restrict the result to entities strictly below [super].
      @param limit The maximum number of entities to return. Default no limit.
      @param cutoff Results with [rank <= cutoff] are excluded. Default 0.0. *)

  val mapping1 : 'a Attribute_type.t -> t -> 'a Values.t Map.t Lwt.t
  (** [mapping1 at e] is a map of values of [at]-attributions with source [e]
      indexed by the attribution targets. *)

  val premapping1 : 'a Attribute_type.t -> t -> 'a Values.t Map.t Lwt.t
  (** [premapping1 at e] is a map of values of [at]-attributions with target [e]
      indexed by the attribution sources. *)

  val connected_by : 'a Attribute_type.t -> 'a -> (t * t) list Lwt.t
  (** [connected_by at vs] is the list of pairs of entities such that there is
      an attribution of type [at] and value [v] from the first entity of the
      pair to the second. The result of a call to this function is not
      cached. *)
end

module type S = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Attribute_uniqueness : ATTRIBUTE_UNIQUENESS
    with module Attribute_type := Attribute_type
  module Relation : RELATION
    with module Attribute_type := Attribute_type
  module Entity_type : ENTITY_TYPE with module Attribute_type := Attribute_type
  module Entity : ENTITY with module Attribute_type := Attribute_type
                          and module Relation := Relation
                          and module Entity_type := Entity_type
end

module type S_SOID = sig
  type soid
  include S
    with type Attribute_type.soid = soid
     and type Attribute_uniqueness.soid = soid
     and type Entity_type.soid = soid
     and type Entity.soid = soid
end
