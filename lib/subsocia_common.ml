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

open Subsocia_prereq
open Panograph_i18n
open Printf
open Unprime_char
open Unprime_list
open Unprime_string

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let invalid_arg_f fmt = ksprintf invalid_arg fmt

module Multiplicity = struct
  type t = May1 | Must1 | May | Must

  let ( * ) a b =
    match a, b with
    | Must1,   _ |    _, Must1 -> b
    | May,     _ |    _, May   -> May
    | May1, May1               -> May1
    | May1, Must | Must, May1  -> May
    | Must, Must               -> Must

  let of_int = function
    | 0 -> May1
    | 1 -> Must1
    | 2 -> May
    | 3 -> Must
    | _ -> invalid_arg "Multiplicity.of_int"

  let to_int = function
    | May1 -> 0
    | Must1 -> 1
    | May -> 2
    | Must -> 3

  let of_char = function
    | '?' -> May1
    | '1' -> Must1
    | '*' -> May
    | '+' -> Must
    | _ -> invalid_arg "Multiplicity.of_char"

  let to_char = function
    | May1 -> '?'
    | Must1 -> '1'
    | May -> '*'
    | Must -> '+'

  let of_string s =
    if String.length s = 1 then of_char s.[0] else
    invalid_arg "Multiplicity.of_string"

  let to_string m = String.make 1 (to_char m)
end

type 'a value_type =
  | Vt_bool : bool value_type
  | Vt_int : int value_type
  | Vt_string : string value_type
  | Vt_twine : Twine.t value_type

type any_value_type = Any_value_type : 'a value_type -> any_value_type

let string_of_value_type : type a. a value_type -> string = function
  | Vt_bool -> "bool"
  | Vt_int -> "int"
  | Vt_string -> "string"
  | Vt_twine -> "twine"

let any_value_type_of_string : string -> any_value_type = function
  | "bool" -> Any_value_type Vt_bool
  | "int" -> Any_value_type Vt_int
  | "string" -> Any_value_type Vt_string
  | "twine" -> Any_value_type Vt_twine
  | _ -> invalid_arg "any_value_type_of_string"

type any_value = Any_value : 'a value_type * 'a -> any_value

let string_of_value : type a. langs: lang list -> a value_type -> a -> string
  = fun ~langs -> function
  | Vt_bool -> (function true -> "true" | false -> "false")
  | Vt_int -> string_of_int
  | Vt_string -> fun s -> s
  | Vt_twine -> Twine.to_string ~langs

let value_of_string : type a. a value_type -> string -> a = function
  | Vt_bool -> (function "true" -> true | "false" -> false
		       | _ -> invalid_arg "value_of_string")
  | Vt_int -> int_of_string
  | Vt_string -> fun s -> s
  | Vt_twine -> fun _ -> assert false (* FIXME *)

module Int32_set = Set.Make (Int32)
module Int32_map = Map.Make (Int32)
