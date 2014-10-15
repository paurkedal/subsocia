(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

type lang = int

val lang_of_string : string -> lang
val string_of_lang : lang -> string

module Twine : sig
  type t
  val make : (lang * string) list -> t
  val to_string : langs: lang list -> t -> string
end

module Multiplicity : sig
  type t =
    | May1
    | Must1
    | May
    | Must

  val ( * ) : t -> t -> t

  val of_int : int -> t
  val to_int : t -> int

  val of_string : string -> t
  val to_string : t -> string

  val of_char : char -> t
  val to_char : t -> char
end

type 'a attribute_type =
  | At_bool : bool attribute_type
  | At_int : int attribute_type
  | At_string : string attribute_type
  | At_twine : Twine.t attribute_type

type 'a attribute_key = string * 'a attribute_type

type exists_attribute_key =
  Exists_attribute_key : 'a attribute_key -> exists_attribute_key

type attribute_info = {
  ai_key : exists_attribute_key;
  ai_name : Twine.t;
}

val string_of_attribute : langs: lang list -> 'a attribute_type -> 'a -> string
