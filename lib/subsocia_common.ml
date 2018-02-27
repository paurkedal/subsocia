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

open Subsocia_prereq
open Panograph_i18n
open Printf
open Unprime_char
open Unprime_list
open Unprime_string

module type SET = Prime_enumset.S_with_monadic with type 'a monad = 'a Lwt.t
module type MAP = Prime_enummap.S_with_monadic with type 'a monad = 'a Lwt.t

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

module Type = struct

  type 'a t =
    | Bool : bool t
    | Int : int t
    | String : string t

  type ex = Ex : 'a t -> ex

  let to_string : type a. a t -> string = function
    | Bool -> "bool"
    | Int -> "int"
    | String -> "string"

  let of_string = function
    | "bool" -> Ex Bool
    | "int" -> Ex Int
    | "string" -> Ex String
    | _ -> invalid_arg "Type.of_string"
end

module Value = struct
  type ex = Ex : 'a Type.t * 'a -> ex

  let typed_to_string : type a. a Type.t -> a -> string =
    function
    | Type.Bool -> (function true -> "true" | false -> "false")
    | Type.Int -> string_of_int
    | Type.String -> fun s -> s

  let typed_of_string : type a. a Type.t -> string -> a =
    function
    | Type.Bool -> (function "true" -> true | "false" -> false
                           | _ -> invalid_arg "Value.Typed_of_string")
    | Type.Int -> int_of_string
    | Type.String -> fun s -> s

  let to_string (Ex (t, v)) = typed_to_string t v

  let coerce : type a. a Type.t -> ex -> a = fun t v ->
    match t, v with
    | Type.Bool, (Ex (Type.Bool, x)) -> x
    | Type.Int, (Ex (Type.Int, x)) -> x
    | Type.String, (Ex (Type.String, x)) -> x
    | _ -> invalid_arg "Subsocia_common.Value.coerce: Type error."

  let to_json : type a. a Type.t -> a ->
                [> `Bool of bool | `Int of int | `String of string] = function
    | Type.Bool -> fun x -> `Bool x
    | Type.Int -> fun x -> `Int x
    | Type.String -> fun x -> `String x

  let of_json
    : type a. a Type.t ->
      [> `Bool of bool | `Int of int | `String of string] -> a = function
    | Type.Bool ->
      (function `Bool (x : bool) -> x | _ -> invalid_arg "Value.of_json")
    | Type.Int ->
      (function `Int (x : int) -> x | _ -> invalid_arg "Value.of_json")
    | Type.String ->
      (function `String (x : string) -> x | _ -> invalid_arg "Value.of_json")

  let to_json_string : type a. a Type.t -> a -> string = function
    | Type.Bool -> fun x -> string_of_bool x
    | Type.Int -> fun x -> string_of_int x
    | Type.String -> fun x -> Yojson.Basic.to_string (`String x)

  let of_json_string : type a. a Type.t -> string -> a = function
    | Type.Bool -> fun s -> bool_of_string (String.trim s)
    | Type.Int -> fun s -> int_of_string (String.trim s)
    | Type.String -> fun s ->
      begin match Yojson.Basic.from_string s with
      | `String s -> s
      | _ -> invalid_arg "Value.of_json_string"
      end

  (**/**)
  let typed_to_poly = to_json
end

module Bool_compare = struct type t = bool let compare = compare end
module Bool_set = Prime_enumset.Make_monadic (Bool_compare) (Lwt) (* TODO:opt *)
module Int_compare = struct type t = int let compare = compare end
module Int_set = Prime_enumset.Make_monadic (Int_compare) (Lwt)
module String_set = Prime_enumset.Make_monadic (String) (Lwt)
module String_map = Prime_enummap.Make_monadic (String) (Lwt)

module Values = struct

  type 'a t =
    T : (module Prime_enumset.S with type elt = 'a and type t = 'b) * 'b -> 'a t

  type ex = Ex : 'a Type.t * 'a t -> ex

  let coerce : type a. a Type.t -> ex -> a t = fun t ex ->
    match t, ex with
    | Type.Bool, Ex (Type.Bool, vs) -> vs
    | Type.Int, Ex (Type.Int, vs) -> vs
    | Type.String, Ex (Type.String, vs) -> vs
    | _ -> failwith "Subsocia_common.Values.must_coerce: Type mismatch."

  let impl : type a. a Type.t -> (module Prime_enumset.S with type elt = a) =
    function
    | Type.Bool ->   (module Bool_set)
    | Type.Int ->    (module Int_set)
    | Type.String -> (module String_set)

  let empty : type a. a Type.t -> a t = fun t ->
    let module S = (val impl t) in
    T ((module S), S.empty)

  let singleton : type a. a Type.t -> a -> a t = fun t v ->
    let module S = (val impl t) in
    T ((module S), S.singleton v)

  let is_empty (type a) (T ((module S), s) : a t) = S.is_empty s
  let contains (type a) (x : a) (T ((module S), s) : a t) = S.contains x s
  let locate (type a) (x : a) (T ((module S), s) : a t) = S.locate x s
  let cardinal (type a) (T ((module S), s) : a t) = S.cardinal s
  let min_elt (type a) (T ((module S), s) : a t) = S.min_elt s
  let max_elt (type a) (T ((module S), s) : a t) = S.max_elt s
  let add (type a) (x : a) (T ((module S), s) : a t) = T ((module S), S.add x s)
  let remove (type a) x (T ((module S), s) : a t) = T ((module S), S.remove x s)
  let iter (type a) f (T ((module S), s) : a t) = S.iter f s
  let fold (type a) f (T ((module S), s) : a t) acc = S.fold f s acc
  let fold_rev (type a) f (T ((module S), s) : a t) acc = S.fold_rev f s acc
  let for_all (type a) f (T ((module S), s) : a t) = S.for_all f s
  let exists (type a) f (T ((module S), s) : a t) = S.exists f s
  let filter (type a) f (T ((module S), s) : a t) = T ((module S), S.filter f s)
  let union (type a) (T ((module SA), sA) : a t) (T ((module SB), sB) : a t) =
    T ((module SB), SA.fold SB.add sA sB)
  let inter (type a) (T ((module SA), sA) : a t) (T ((module SB), sB) : a t) =
    T ((module SB), SB.filter (fun x -> SA.contains x sA) sB)
  let choose (type a) (T ((module S), s) : a t) = S.choose s
  let elements (type a) (T ((module S), s) : a t) = S.elements s

  let of_elements : type a. a Type.t -> a list -> a t = fun t xs ->
    let module S = (val impl t) in
    T ((module S), List.fold S.add xs S.empty)

  let of_ordered_elements : type a. a Type.t -> a list -> a t = fun t s ->
    let module S = (val impl t) in
    T ((module S), S.of_ordered_elements s)

  let to_json t vs =
    `List (fold_rev (fun v acc -> Value.to_json t v :: acc) vs [])

  let of_json t = function
    | `List jvs -> List.fold (fun jv -> add (Value.of_json t jv)) jvs (empty t)
    | _ -> invalid_arg "Values.of_json"

  let to_json_string t vs = Yojson.Basic.to_string (to_json t vs)

  let of_json_string t s = of_json t (Yojson.Basic.from_string s)
end

module Int32_set = Prime_enumset.Make_monadic (Int32) (Lwt)
module Int32_map = Prime_enummap.Make_monadic (Int32) (Lwt)

module Int32_hashable = struct
  type t = int32
  let equal = (=)
  let hash = Hashtbl.hash
end
module Int32_event_table = Panograph_event_table.Make (Int32_hashable)
