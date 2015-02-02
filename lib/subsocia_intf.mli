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

(* TODO: Restrict these. *)
module type SET = Prime_enumset.S
module type MAP = Prime_enummap.S

module type S = sig

  module Attribute_key : sig
    type 'a t1
    type t0 = Ex : 'a t1 -> t0

    module Set : SET with type elt = t0
    module Map : MAP with type key = t0

    val of_name : string -> t0 Lwt.t
    val name : t0 -> string Lwt.t
    val of_id : int32 -> t0 Lwt.t
    val id : t0 -> int32
    val type0 : t0 -> Type.t0
    val type1 : 'a t1 -> 'a Type.t1
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
    val inclusion_preds : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
    val inclusion_succs : t -> (Multiplicity.t * Multiplicity.t) Map.t Lwt.t
    val attribution : t -> t -> Multiplicity.t Attribute_key.Map.t Lwt.t
(*
    val attribution_preds : t -> Multiplicity.t Attribute_key.Map.t Map.t Lwt.t
    val attribution_succs : t -> Multiplicity.t Attribute_key.Map.t Map.t Lwt.t
*)
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

    val minimums : unit -> Set.t Lwt.t
    val maximums : unit -> Set.t Lwt.t
    val preds : t -> Set.t Lwt.t
    val succs : t -> Set.t Lwt.t
    val precedes : t -> t -> bool Lwt.t

    val fetch_attribute : t -> t -> 'a Attribute_key.t1 -> 'a list Lwt.t
    val store_attribute : t -> t -> 'a Attribute_key.t1 -> 'a list -> unit Lwt.t
    val display_name : langs: lang list -> t -> string Lwt.t

    val constrain : t -> t -> unit Lwt.t
    val unconstrain : t -> t -> unit Lwt.t
  end
end
