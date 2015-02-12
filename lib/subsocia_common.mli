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

  val t_of_rpc : Rpc.t -> t
  val rpc_of_t : t -> Rpc.t
end

module Type : sig

  type 'a t1 =
    | Bool : bool t1
    | Int : int t1
    | String : string t1

  type t0 = Ex : 'a t1 -> t0

  val to_string : 'a t1 -> string
  val of_string : string -> t0

(*
  val string_of_value_fun : 'a t1 -> 'a -> string
  val value_of_string_fun : 'a t1 -> string -> 'a
*)

  val rpc_of_t0 : t0 -> Rpc.t
  val t0_of_rpc : Rpc.t -> t0
end

module Value : sig
  type t0 = Ex : 'a Type.t1 * 'a -> t0

  val typed_to_string : langs: lang list -> 'a Type.t1 -> 'a -> string

  val to_string : langs: lang list -> t0 -> string
(*
  val of_string : string -> t0
*)

  val coerce : 'a Type.t1 -> t0 -> 'a

  val rpc_of_t0 : t0 -> Rpc.t
  val t0_of_rpc : Rpc.t -> t0
end

module Int32_set : Prime_enumset.S with type elt = int32
module Int32_map : Prime_enummap.S with type key = int32
