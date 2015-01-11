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

val (>>=) : 'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
val (>|=) : 'a Lwt.t -> ('a -> 'b) -> 'b Lwt.t

module Multiplicity : sig
  type t = May1 | Must1 | May | Must

  val ( * ) : t -> t -> t

  val of_int : int -> t
  val to_int : t -> int

  val of_string : string -> t
  val to_string : t -> string

  val of_char : char -> t
  val to_char : t -> char
end

type 'a value_type =
  | Vt_bool : bool value_type
  | Vt_int : int value_type
  | Vt_string : string value_type
  | Vt_twine : Twine.t value_type

type any_value_type = Any_value_type : 'a value_type -> any_value_type

val string_of_value_type : 'a value_type -> string
val any_value_type_of_string : string -> any_value_type

type any_value = Any_value : 'a value_type * 'a -> any_value

val string_of_value : langs: lang list -> 'a value_type -> 'a -> string

val value_of_string : 'a value_type -> string -> 'a

module Int32_set : Set.S with type elt = int32
module Int32_map : Map.S with type key = int32
