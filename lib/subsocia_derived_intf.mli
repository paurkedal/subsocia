(* Copyright (C) 2015--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Signature for the derived API. *)

open Iso639
open Subsocia_common
open Subsocia_selector_types

module type ITERABLE = sig
  type t

  val fold_s : ?time: Ptime.t -> (t -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t
  val iter_s : ?time: Ptime.t -> (t -> unit Lwt.t) -> t -> unit Lwt.t
  val for_all_s : ?time: Ptime.t -> (t -> bool Lwt.t) -> t -> bool Lwt.t
  val exists_s : ?time: Ptime.t -> (t -> bool Lwt.t) -> t -> bool Lwt.t
  val find_map_s :
    ?time: Ptime.t -> (t -> 'a option Lwt.t) -> t -> 'a option Lwt.t
  val search_s :
    ?time: Ptime.t -> (t -> 'a option Lwt.t) -> t -> 'a option Lwt.t
  [@@deprecated "Renamed to find_map_s."]
end

module type NESTED_ITERABLE = sig
  type t

  exception Prune

  val fold_s :
    ?time: Ptime.t -> ?max_depth: int ->
    (t -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t

  val iter_s :
    ?time: Ptime.t -> ?max_depth: int -> (t -> unit Lwt.t) -> t -> unit Lwt.t

  val for_all_s :
    ?time: Ptime.t -> ?max_depth: int -> (t -> bool Lwt.t) -> t -> bool Lwt.t

  val exists_s :
    ?time: Ptime.t -> ?max_depth: int -> (t -> bool Lwt.t) -> t -> bool Lwt.t

  val find_map_s :
    ?time: Ptime.t -> ?max_depth: int ->
    (t -> 'a option Lwt.t) -> t -> 'a option Lwt.t

  val search_s :
    ?time: Ptime.t -> ?max_depth: int ->
    (t -> 'a option Lwt.t) -> t -> 'a option Lwt.t
  [@@deprecated "Renamed to find_map_s."]
end

module type ATTRIBUTE_TYPE = Subsocia_intf.ATTRIBUTE_TYPE

module type ATTRIBUTE_UNIQUENESS = sig
  include Subsocia_intf.ATTRIBUTE_UNIQUENESS

  val soid_string : t -> string Lwt.t
end

module type RELATION = sig
  include Subsocia_intf.RELATION

  val (&&) : t -> t -> t
  val inter : t list -> t
  val true_ : t
  val present : 'a Attribute_type.t -> t
  val (=) : 'a Attribute_type.t -> 'a -> t
  val (<:) : 'a Attribute_type.t -> 'a Values.t -> t
  val (<::) : 'a Attribute_type.t -> 'a list -> t
  val (<=) : 'a Attribute_type.t -> 'a -> t
  val (>=) : 'a Attribute_type.t -> 'a -> t
  val between : 'a Attribute_type.t -> 'a -> 'a -> t
  val search : string Attribute_type.t -> Subsocia_re.t -> t
  val search_fts : Subsocia_fts.t -> t

  val to_selector : t -> selector Lwt.t
  (** [to_selector r] constructs a one-step selector corresponding to [r] if
      possible.
      @raise Subsocia_error.Exn if [r] contains a search term. *)
end

module type ENTITY_TYPE = sig
  include Subsocia_intf.ENTITY_TYPE

  val required : string -> t Lwt.t
  val equal : t -> t -> bool
end

module type ENTITY = sig
  include Subsocia_intf.ENTITY

  module Attribute_uniqueness : ATTRIBUTE_UNIQUENESS
    with module Attribute_type := Attribute_type

  val soid_string : t -> string Lwt.t

  val equal : t -> t -> bool

  val force_sub : t -> t -> unit Lwt.t

  val select_from : ?time: Ptime.t -> selector -> Set.t -> Set.t Lwt.t
  val select : ?time: Ptime.t -> selector -> Set.t Lwt.t
  val select_opt : ?time: Ptime.t -> selector -> t option Lwt.t
  val select_one : ?time: Ptime.t -> selector -> t Lwt.t

  (* TODO: Replace and deprecate these. *)
  val getattr_opt : t -> t -> 'a Attribute_type.t -> 'a option Lwt.t
  val getattr_one : t -> t -> 'a Attribute_type.t -> 'a Lwt.t

  val add_value : 'a Attribute_type.t -> 'a -> t -> t -> unit Lwt.t
  val remove_value : 'a Attribute_type.t -> 'a -> t -> t -> unit Lwt.t
  val set_value : 'a Attribute_type.t -> 'a -> t -> t -> unit Lwt.t
  val clear_values : 'a Attribute_type.t -> t -> t -> unit Lwt.t

  val of_unique_name : ?super: t -> string -> t option Lwt.t

  module Dsuper : ITERABLE with type t := t
  module Dsub : ITERABLE with type t := t
  module Super : NESTED_ITERABLE with type t := t
  module Sub : NESTED_ITERABLE with type t := t

  val unique_mapping1 : Attribute_uniqueness.t -> t -> Relation.t Map.t Lwt.t
  (** Given a uniqueness constraint [au], [unique_mapping1 au e] returns a map
      from entities which have a unique image at [e] to the corresponding
      relations. *)

  val unique_premapping1 : Attribute_uniqueness.t -> t -> Relation.t Map.t Lwt.t
  (** Given a uniqueness constraint [au], [unique_premapping1 au e] returns a
      map from entities which have [e] as a unique image to the the
      corresponding relation. *)

  val unique_relations_by_image : t -> Relation.t list Map.t Lwt.t
  (** [unique_relations_by_image e] returns a map of all bindings [(e', rel)]
      such that [e'] is the image of [e] under [rel], where [rel] is composed
      according to some attribute uniqueness constraint, guaranteeing the
      uniqueness of [e'] given [e]. *)

  val unique_relations_by_preimage : t -> Relation.t list Map.t Lwt.t
  (** [unique_relations_by_preimage] returns a map of all bindings [(e', rel)]
      such that [e] is the image of [e'] under [rel], where [rel] is composed
      according to some attribute uniqueness constraint, guaranteeing the
      uniqueness of [e] given [e'].  Alternatively [e'] is the preimage of [e],
      which motivates the function name.  Note the asymmetry with
      {!unique_relations_by_image} regarding the uniqueness constraint. *)

  val paths : t -> selector list Lwt.t

  val has_role_for_entity : string -> t -> t -> bool Lwt.t
  val can_view_entity : t -> t -> bool Lwt.t
  val can_edit_entity : t -> t -> bool Lwt.t
  val can_search_below : t -> t -> bool Lwt.t

  val display_name : ?context: Set.t -> ?langs: Lang.t list -> t -> string Lwt.t
  val candidate_dsupers : ?include_current: bool -> t -> Set.t Lwt.t
end

module type S = sig
  module Attribute_type : ATTRIBUTE_TYPE

  module Attribute_uniqueness : ATTRIBUTE_UNIQUENESS
    with module Attribute_type := Attribute_type

  module Relation : RELATION
    with module Attribute_type := Attribute_type

  module Entity_type : ENTITY_TYPE
    with module Attribute_type := Attribute_type

  module Entity : ENTITY
    with module Attribute_type := Attribute_type
     and module Attribute_uniqueness := Attribute_uniqueness
     and module Relation := Relation
     and module Entity_type := Entity_type

  module Const : sig
    val at_unique_name : string Attribute_type.t Lwt.t
    val at_proper_name : string Attribute_type.t Lwt.t
    val at_first_name : string Attribute_type.t Lwt.t
    val at_last_name : string Attribute_type.t Lwt.t
    val at_email : string Attribute_type.t Lwt.t
    val at_role : string Attribute_type.t Lwt.t

    val et_root : Entity_type.t Lwt.t
    val et_access_group : Entity_type.t Lwt.t
    val et_auth_group : Entity_type.t Lwt.t
    val et_person : Entity_type.t Lwt.t

    val e_new_users : Entity.t Lwt.t
    val e_default : Entity.t Lwt.t
  end [@@ocaml.deprecated]
end

module type S_SOID = sig
  type soid
  include S
    with type Attribute_type.soid = soid
     and type Attribute_uniqueness.soid = soid
     and type Entity_type.soid = soid
     and type Entity.soid = soid
end
