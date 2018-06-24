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

  type any = Any : 'a t -> any
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

  let any_of_string = function
    | "bool" -> Any Bool
    | "int" -> Any Int
    | "string" -> Any String
    | _ -> invalid_arg "Type.any_of_string"

  let enum : type a. a t -> int = function
   | Bool -> 0
   | Int -> 1
   | String -> 2

  let equal (type a b) (t1 : a t) (t2 : b t) =
    (match t1, t2 with
     | Bool, Bool -> true
     | Bool, _ -> false
     | Int, Int -> true
     | Int, _ -> false
     | String, String -> true
     | String, _ -> false)

  let compare t1 t2 = compare (enum t1) (enum t2)

  let pp ppf vt = Format.pp_print_string ppf (to_string vt)
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
end

module Bool_compare = struct type t = bool let compare = compare end
module Bool_set = Prime_enumset.Make_monadic (Bool_compare) (Lwt) (* TODO:opt *)
module Int_compare = struct type t = int let compare = compare end
module Int_set = Prime_enumset.Make_monadic (Int_compare) (Lwt)
module String_set = Prime_enumset.Make_monadic (String) (Lwt)
module String_map = Prime_enummap.Make_monadic (String) (Lwt)

module Values = struct

  type 'a t =
    | Bool : Bool_set.t -> bool t
    | Int : Int_set.t -> int t
    | String : String_set.t -> string t

  type any = Any : 'a t -> any
  type ex = Ex : 'a t -> ex

  let coerce : type a. a Type.t -> ex -> a t = fun typ ex ->
    (match typ, ex with
     | Type.Bool, Ex (Bool _ as xs) -> xs
     | Type.Int, Ex (Int _ as xs) -> xs
     | Type.String, Ex (String _ as xs) -> xs
     | _ -> failwith "Subsocia_common.Values.coerce: Type mismatch.")

  let coerce_any : type a. a Type.t -> any -> a t option = fun typ any ->
    (match typ, any with
     | Type.Bool, Any (Bool _ as xs) -> Some xs
     | Type.Int, Any (Int _ as xs) -> Some xs
     | Type.String, Any (String _ as xs) -> Some xs
     | _ -> None)

  let empty : type a. a Type.t -> a t = function
   | Type.Bool -> Bool Bool_set.empty
   | Type.Int -> Int Int_set.empty
   | Type.String -> String String_set.empty

  let singleton : type a. a Type.t -> a -> a t = function
   | Type.Bool -> fun x -> Bool (Bool_set.singleton x)
   | Type.Int -> fun x -> Int (Int_set.singleton x)
   | Type.String -> fun x -> String (String_set.singleton x)

  type ('set, 'elt) set = (module SET with type t = 'set and type elt = 'elt)

  type ('e, 'a) prop = {v: 's. ('s, 'e) set -> 's -> 'a}
  let lift_prop : type e. (e, 'a) prop -> e t -> 'a =
    fun f -> function
     | Bool xs -> f.v (module Bool_set) xs
     | Int xs -> f.v (module Int_set) xs
     | String xs -> f.v (module String_set) xs

  type ('e, 'a) prop2 = {v: 's. ('s, 'e) set -> 's -> 's -> 'a}
  let lift_prop2 : type e. (e, 'a) prop2 -> e t -> e t -> 'a =
    fun f s1 s2 ->
    (match s1, s2 with
     | Bool xs, Bool ys -> f.v (module Bool_set) xs ys
     | Int xs, Int ys -> f.v (module Int_set) xs ys
     | String xs, String ys -> f.v (module String_set) xs ys)

  type 'e endo = {v: 's. ('s, 'e) set -> 's -> 's}
  let lift_endo : type e. e endo -> e t -> e t =
    fun f -> function
     | Bool xs -> Bool (f.v (module Bool_set) xs)
     | Int xs -> Int (f.v (module Int_set) xs)
     | String xs -> String (f.v (module String_set) xs)

  type 'e bina = {v: 's. ('s, 'e) set -> 's -> 's -> 's}
  let lift_bina : type e. e bina -> e t -> e t -> e t =
    fun f a b ->
    (match a, b with
     | Bool xs, Bool ys -> Bool (f.v (module Bool_set) xs ys)
     | Int xs, Int ys -> Int (f.v (module Int_set) xs ys)
     | String xs, String ys -> String (f.v (module String_set) xs ys))

  let is_empty (type a) s =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.is_empty s} s
  let add (type a) x =
    lift_endo {v = fun (type s) ((module S) : (s, a) set) s -> S.add x s}
  let remove (type a) x =
    lift_endo {v = fun (type s) ((module S) : (s, a) set) s -> S.remove x s}
  let mem (type a) x =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.mem x s}
  let locate (type a) x =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.locate x s}
  let cardinal (type a) s =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.cardinal s} s
  let min_elt_exn (type a) s =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.min_elt_exn s} s
  let max_elt_exn (type a) s =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.max_elt_exn s} s
  let iter (type a) f =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.iter f s}
  let fold (type a) f =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.fold f s}
  let fold_rev (type a) f =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.fold_rev f s}
  let exists (type a) f =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.exists f s}
  let for_all (type a) f =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.for_all f s}
  let filter (type a) f =
    lift_endo {v = fun (type s) ((module S) : (s, a) set) s -> S.filter f s}
  let union (type a) s =
    lift_bina {v = fun (type s) ((module S) : (s, a) set) s t -> S.union s t} s
  let inter (type a) s =
    lift_bina {v = fun (type s) ((module S) : (s, a) set) s t -> S.inter s t} s
  let choose (type a) s =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.choose s} s
  let elements (type a) s =
    lift_prop {v = fun (type s) ((module S) : (s, a) set) s -> S.elements s} s
  let equal (type a) s =
    lift_prop2 {v = fun (type s) ((module S) : (s, a) set) s t -> S.equal s t} s
  let compare (type a) s =
    lift_prop2 {v = fun (type s) ((module S) : (s, a) set) s t -> S.compare s t} s

  let of_elements : type e. e Type.t -> e list -> e t = fun t xs ->
    (match t with
     | Type.Bool -> Bool (List.fold Bool_set.add xs Bool_set.empty)
     | Type.Int -> Int (List.fold Int_set.add xs Int_set.empty)
     | Type.String -> String (List.fold String_set.add xs String_set.empty))

  let of_ordered_elements : type e. e Type.t -> e list -> e t = function
   | Type.Bool -> fun xs -> Bool (Bool_set.of_ordered_elements xs)
   | Type.Int -> fun xs -> Int (Int_set.of_ordered_elements xs)
   | Type.String -> fun xs -> String (String_set.of_ordered_elements xs)

  let to_json t vs =
    `List (fold_rev (fun v acc -> Value.to_json t v :: acc) vs [])

  let of_json t = function
    | `List jvs -> List.fold (fun jv -> add (Value.of_json t jv)) jvs (empty t)
    | _ -> invalid_arg "Values.of_json"

  let to_json_string t vs = Yojson.Basic.to_string (to_json t vs)

  let of_json_string t s = of_json t (Yojson.Basic.from_string s)

  let contains = mem
  let min_elt = min_elt_exn
  let max_elt = max_elt_exn
end

module Int32_set = Prime_enumset.Make_monadic (Int32) (Lwt)
module Int32_map = Prime_enummap.Make_monadic (Int32) (Lwt)

module Int32_hashable = struct
  type t = int32
  let equal = (=)
  let hash = Hashtbl.hash
end
module Int32_event_table = Panograph_event_table.Make (Int32_hashable)
