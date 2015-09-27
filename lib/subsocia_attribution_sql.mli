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

open Caqti_query
open Subsocia_intf

module type Arg = sig
  module Attribute_type : ATTRIBUTE_TYPE
  module Attribute : ATTRIBUTE with module Attribute_type := Attribute_type
end

module Make (Arg : Arg) : sig
  open Arg

  val select_image : Attribute.predicate -> int32 list -> query
  val select_preimage : Attribute.predicate -> int32 list -> query
end
