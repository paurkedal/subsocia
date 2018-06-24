(* Copyright (C) 2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open Subsocia_common

type t =
  [ `Msg of string
  | `Attribute_type_missing of string
  | `Attribute_type_mismatch of string * Type.any * Type.any
  | `Entity_type_missing of string ]

exception Exn of t
(** Exception raised by various functions suffixed by [_exn]. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf error] prints a human readable presenation of [error] to [ppf]. *)

val show : t -> string
(** [show error] is a human readable presentation of [error]. *)

(**/**) (* internal use *)
val fail_lwt : ('a, Format.formatter, unit, 'b Lwt.t) format4 -> 'a
