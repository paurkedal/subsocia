(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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
open Subsocia_intf

module Make (Base : Subsocia_intf.S) : sig
  open Base

  module Attribute_type : sig
    type 'a t1 = 'a Base.Attribute_type.t1
    type t0 = Base.Attribute_type.t0 = Ex : 'a t1 -> t0
    include ATTRIBUTE_TYPE
       with type 'a t1 := 'a t1 and type t0 := t0
	and module Map = Base.Attribute_type.Map

    val coerce : 'a Type.t1 -> t0 -> 'a t1 option
  end

  module Attribute : sig
    type t0 = Ex : 'a Attribute_type.t1 * 'a -> t0
  end

  module Entity_type :
    ENTITY_TYPE with module Attribute_type := Base.Attribute_type
		 and type t = Base.Entity_type.t
		 and module Set = Base.Entity_type.Set
		 and module Map = Base.Entity_type.Map

  module Const : sig
    val at_unique_name : string Base.Attribute_type.t1 Lwt.t
    val at_proper_name : string Base.Attribute_type.t1 Lwt.t
    val at_first_name : string Base.Attribute_type.t1 Lwt.t
    val at_last_name : string Base.Attribute_type.t1 Lwt.t
    val at_email : string Base.Attribute_type.t1 Lwt.t

    val et_unit : Base.Entity_type.t Lwt.t
    val et_access_group : Base.Entity_type.t Lwt.t
    val et_auth_group : Base.Entity_type.t Lwt.t
    val et_person : Base.Entity_type.t Lwt.t

    val e_forbidden : Base.Entity.t Lwt.t
    val e_default_viewers : Base.Entity.t Lwt.t
    val e_default_admins : Base.Entity.t Lwt.t
    val e_new_users : Base.Entity.t Lwt.t
  end

  module Entity : sig
    include ENTITY with module Attribute_type := Base.Attribute_type
		    and module Entity_type := Base.Entity_type
		    and type t = Base.Entity.t
		    and module Set = Base.Entity.Set
		    and module Map = Base.Entity.Map

    val of_unique_name : ?super: Base.Entity.t -> string ->
			 Base.Entity.t option Lwt.t
    val display_name : langs: lang list -> Base.Entity.t -> string Lwt.t
    val candidate_succs : Base.Entity.t -> Base.Entity.Set.t Lwt.t
  end

end
