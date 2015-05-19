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

(** Signature for the derived API. *)

open Panograph_i18n
open Subsocia_common
open Subsocia_intf
open Subsocia_selector_types

module type ITERABLE = sig
  type t

  val fold_s : (t -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t
  val iter_s : (t -> unit Lwt.t) -> t -> unit Lwt.t
  val for_all_s : (t -> bool Lwt.t) -> t -> bool Lwt.t
  val exists_s : (t -> bool Lwt.t) -> t -> bool Lwt.t
  val search_s : (t -> 'a option Lwt.t) -> t -> 'a option Lwt.t
end

module type NESTED_ITERABLE = sig
  type t

  exception Prune

  val fold_s : ?max_depth: int -> (t -> 'a -> 'a Lwt.t) -> t -> 'a -> 'a Lwt.t
  val iter_s : ?max_depth: int -> (t -> unit Lwt.t) -> t -> unit Lwt.t
  val for_all_s : ?max_depth: int -> (t -> bool Lwt.t) -> t -> bool Lwt.t
  val exists_s : ?max_depth: int -> (t -> bool Lwt.t) -> t -> bool Lwt.t
  val search_s : ?max_depth: int ->
		 (t -> 'a option Lwt.t) -> t -> 'a option Lwt.t
end

module type S = sig
  module Base : Subsocia_intf.S

  open Base

  module Attribute_type : sig
    type 'a t1 = 'a Base.Attribute_type.t1
    type t0 = Base.Attribute_type.t0 = Ex : 'a t1 -> t0
    include ATTRIBUTE_TYPE
       with type 'a t1 := 'a t1 and type t0 := t0
	and module Map = Base.Attribute_type.Map

    val coerce : 'a Type.t1 -> t0 -> 'a t1 option
  end

  module Entity_type :
    ENTITY_TYPE with module Attribute_type := Base.Attribute_type
		 and type t = Base.Entity_type.t
		 and module Set = Base.Entity_type.Set
		 and module Map = Base.Entity_type.Map

  module Entity : sig
    include ENTITY with module Attribute_type := Base.Attribute_type
		    and module Attribute := Base.Attribute
		    and module Entity_type := Base.Entity_type
		    and type t = Base.Entity.t
		    and module Set = Base.Entity.Set
		    and module Map = Base.Entity.Map

    val force_sub : t -> t -> unit Lwt.t

    val select_from : selector -> Set.t -> Set.t Lwt.t
    val select : selector -> Set.t Lwt.t
    val select_opt : selector -> t option Lwt.t
    val select_one : selector -> t Lwt.t

    val getattr_opt : t -> t -> 'a Attribute_type.t1 -> 'a option Lwt.t
    val getattr_one : t -> t -> 'a Attribute_type.t1 -> 'a Lwt.t

    val of_unique_name : ?super: t -> string -> t option Lwt.t

    module Dsuper : ITERABLE with type t := t
    module Dsub : ITERABLE with type t := t
    module Super : NESTED_ITERABLE with type t := t
    module Sub : NESTED_ITERABLE with type t := t

    val paths : t -> selector list Lwt.t

    val has_role : string -> t -> t -> bool Lwt.t
    val can_view : t -> t -> bool Lwt.t
    val can_edit : t -> t -> bool Lwt.t

    val display_name : langs: lang list -> t -> string Lwt.t
    val candidate_dsupers : t -> Set.t Lwt.t

    val precedes : t -> t -> bool Lwt.t
    (** @deprecated Use is_sub. *)
  end

  module Const : sig
    val at_unique_name : string Attribute_type.t1 Lwt.t
    val at_proper_name : string Attribute_type.t1 Lwt.t
    val at_first_name : string Attribute_type.t1 Lwt.t
    val at_last_name : string Attribute_type.t1 Lwt.t
    val at_email : string Attribute_type.t1 Lwt.t

    val et_unit : Entity_type.t Lwt.t
    val et_access_group : Entity_type.t Lwt.t
    val et_auth_group : Entity_type.t Lwt.t
    val et_person : Entity_type.t Lwt.t

    val e_forbidden : Entity.t Lwt.t
    val e_new_users : Entity.t Lwt.t
  end
end
