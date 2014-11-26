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

open Subsocia_prereq
open Printf
open Unprime_char
open Unprime_list
open Unprime_string

let invalid_arg_f fmt = ksprintf invalid_arg fmt

type lang = int

let lang_of_string s =
  let int_of_letter c = Char.code (Char.lowercase c) - 0x60 in
  let len = String.length s in
  if len < 2 || len > 4 || not (String.for_all Char.is_alpha s) then
    invalid_arg_f "lang_of_string: %s is not a valid ISO 639 language code." s;
  let _, lang =
    String.fold (fun c (k, lang) -> (k - 6, lang lor int_of_letter c lsl k))
		s (18, 0) in
  lang

let string_of_lang lang =
  let letter_of_int i = Char.chr (i + 0x60) in
  let len =
    if lang land 0xfff = 0 then if lang land 0x03ffff = 0 then 1 else 2
			   else if lang land 0x00003f = 0 then 3 else 4 in
  String.sample (fun i -> letter_of_int (lang lsr (18 - 6 * i) land 0x3f)) len

module Twine = struct
  type t = string Int_map.t

  let make lms =
    List.fold (fun (lang, msg) -> Int_map.add lang msg) lms Int_map.empty

  let equal = Int_map.equal (=)

  let compare = Int_map.compare String.compare

  let rec to_string ~langs tw =
    match langs with
    | [] -> raise Not_found
    | lang :: langs -> try Int_map.find lang tw
		       with Not_found -> to_string ~langs tw
end

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

let string_of_attribute
  : type a. langs: lang list -> a attribute_type -> a -> string
  = fun ~langs -> function
  | At_bool -> (function true -> "true" | false -> "false")
  | At_int -> string_of_int
  | At_string -> fun s -> s
  | At_twine -> Twine.to_string ~langs

let attribute_of_string : type a. a attribute_type -> string -> a = function
  | At_bool -> (function "true" -> true | "false" -> false
		       | _ -> invalid_arg "attribute_of_string")
  | At_int -> int_of_string
  | At_string -> fun s -> s
  | At_twine -> fun _ -> assert false (* FIXME *)
