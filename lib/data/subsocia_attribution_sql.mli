(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Subsocia_intf

module type Arg = sig
  module Attribute_type : ATTRIBUTE_TYPE
    with type soid = int32
  module Relation : RELATION
    with module Attribute_type := Attribute_type
end

module Make (Arg : Arg) : sig
  open Arg

  type request =
    | Empty
    | Request :
        ('a, int32, Caqti_mult.zero_or_more) Caqti_request.t * 'a -> request

  val select_image : Relation.t -> int32 list -> request Lwt.t
  val select_preimage : Relation.t -> int32 list -> request Lwt.t
end
