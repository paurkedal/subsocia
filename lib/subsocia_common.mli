(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Common types, modules, and utilities. *)

open Panograph_i18n

module type SET = Prime_enumset.S_with_monadic with type 'a monad = 'a Lwt.t
module type MAP = Prime_enummap.S_with_monadic with type 'a monad = 'a Lwt.t

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

module Type : sig

  type 'a t =
    | Bool : bool t
    | Int : int t
    | String : string t

  type ex = Ex : 'a t -> ex

  val to_string : 'a t -> string
  val of_string : string -> ex
end

module Value : sig
  type ex = Ex : 'a Type.t * 'a -> ex

  val typed_to_string : 'a Type.t -> 'a -> string
  val typed_of_string : 'a Type.t -> string -> 'a

  val to_string : ex -> string

  val coerce : 'a Type.t -> ex -> 'a

  val to_json : 'a Type.t ->
    'a -> [> `Bool of bool | `Int of int | `String of string]

  val of_json : 'a Type.t ->
    [> `Bool of bool | `Int of int | `String of string] -> 'a

  val to_json_string : 'a Type.t -> 'a -> string

  val of_json_string : 'a Type.t -> string -> 'a

  (**/**)
  val typed_to_poly : 'a Type.t -> 'a ->
                      [> `Bool of bool | `Int of int | `String of string]
    [@@ocaml.deprecated "Renamed to to_json."]
end

module Values : sig
  type 'a t
  type ex = Ex : 'a Type.t * 'a t -> ex

  val empty : 'a Type.t -> 'a t
  val is_empty : 'a t -> bool
  val singleton : 'a Type.t -> 'a -> 'a t
  val add : 'a -> 'a t -> 'a t
  val remove : 'a -> 'a t -> 'a t
  val contains : 'a -> 'a t -> bool
  val locate : 'a -> 'a t -> bool * int
  val cardinal : 'a t -> int
  val min_elt : 'a t -> 'a
  val max_elt : 'a t -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : ('a -> bool) -> 'a t -> bool
  val exists : ('a -> bool) -> 'a t -> bool
  val filter : ('a -> bool) -> 'a t -> 'a t
  val union : 'a t -> 'a t -> 'a t
  val inter : 'a t -> 'a t -> 'a t
  val choose : 'a t -> 'a
  val elements : 'a t -> 'a list
  val of_elements : 'a Type.t -> 'a list -> 'a t
  val of_ordered_elements : 'a Type.t -> 'a list -> 'a t

  val coerce : 'a Type.t -> ex -> 'a t

  val to_json : 'a Type.t ->
    'a t ->
    [> `List of [> `Bool of bool | `Int of int | `String of string] list]

  val of_json : 'a Type.t ->
    [> `List of [> `Bool of bool | `Int of int | `String of string] list] ->
    'a t

  val to_json_string : 'a Type.t -> 'a t -> string

  val of_json_string : 'a Type.t -> string -> 'a t
end

module Int_set : SET with type elt = int
module Int32_set : SET with type elt = int32
module Int32_map : MAP with type key = int32
module String_set : SET with type elt = string
module String_map : MAP with type key = string
module Int32_event_table : Panograph_event_table.S with type key = int32
