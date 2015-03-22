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

  val t_of_rpc : Rpc.t -> t
  val rpc_of_t : t -> Rpc.t
end

module Type : sig

  type 'a t1 =
    | Bool : bool t1
    | Int : int t1
    | String : string t1

  type t0 = Ex : 'a t1 -> t0

  val string_of_t0 : t0 -> string
  val string_of_t1 : 'a t1 -> string
  val of_string : string -> t0

  val rpc_of_t0 : t0 -> Rpc.t
  val t0_of_rpc : Rpc.t -> t0
end

module Value : sig
  type t0 = Ex : 'a Type.t1 * 'a -> t0

  val typed_to_string : 'a Type.t1 -> 'a -> string
  val typed_of_string : 'a Type.t1 -> string -> 'a

  val to_string : t0 -> string
(*
  val of_string : string -> t0
*)

  val coerce : 'a Type.t1 -> t0 -> 'a

  val typed_to_poly : 'a Type.t1 -> 'a ->
		      [> `Bool of bool | `Int of int | `String of string]

  val rpc_of_t0 : t0 -> Rpc.t
  val t0_of_rpc : Rpc.t -> t0
end

module Values : sig
  type 'a t

  val empty : 'a Type.t1 -> 'a t
  val is_empty : 'a t -> bool
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
  val elements : 'a t -> 'a list
  val of_ordered_elements : 'a Type.t1 -> 'a list -> 'a t
end

module Int_set : SET with type elt = int
module Int32_set : SET with type elt = int32
module Int32_map : MAP with type key = int32
module String_set : SET with type elt = string
module String_map : MAP with type key = string
module Int32_event_table : Panograph_event_table.S with type key = int32
