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

module type SET = Prime_enumset.S_with_monadic with type 'a monad = 'a Lwt.t
module type MAP = Prime_enummap.S_with_monadic with type 'a monad = 'a Lwt.t

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module Lwtx_list = struct
  let rec search_s f = function
    | [] -> Lwt.return_none
    | x :: xs ->
      f x >>= function None -> search_s f xs
		     | Some _ as r -> Lwt.return r
end

let invalid_arg_f fmt = ksprintf invalid_arg fmt

module Multiplicity = struct
  type t = May1 | Must1 | May | Must with rpc

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

  type 'a t1 =
    | Bool : bool t1
    | Int : int t1
    | String : string t1

  type t0 = Ex : 'a t1 -> t0

  let string_of_t1 : type a. a t1 -> string = function
    | Bool -> "bool"
    | Int -> "int"
    | String -> "string"

  let string_of_t0 (Ex t) = string_of_t1 t

  let of_string = function
    | "bool" -> Ex Bool
    | "int" -> Ex Int
    | "string" -> Ex String
    | _ -> invalid_arg "Type.of_string"

  let rpc_of_t0 t = Rpc.rpc_of_string (string_of_t0 t)
  let t0_of_rpc r = of_string (Rpc.string_of_rpc r)

end

module Value = struct
  type t0 = Ex : 'a Type.t1 * 'a -> t0

  let typed_to_string : type a. a Type.t1 -> a -> string =
    function
    | Type.Bool -> (function true -> "true" | false -> "false")
    | Type.Int -> string_of_int
    | Type.String -> fun s -> s

  let to_string (Ex (t, v)) = typed_to_string t v

  let coerce : type a. a Type.t1 -> t0 -> a = fun t v ->
    match t, v with
    | Type.Bool, (Ex (Type.Bool, x)) -> x
    | Type.Int, (Ex (Type.Int, x)) -> x
    | Type.String, (Ex (Type.String, x)) -> x
    | _ -> invalid_arg "Subsocia_common.Value.coerce: Type error."

  let rpc_of_t0 = function
    | Ex (Type.Bool, x) -> Rpc.rpc_of_bool x
    | Ex (Type.Int, x) -> Rpc.rpc_of_int x
    | Ex (Type.String, x) -> Rpc.rpc_of_string x

  let t0_of_rpc rpc =
    match Rpc.t_of_rpc rpc with
    | Rpc.Bool x -> Ex (Type.Bool, x)
    | Rpc.Int x -> Ex (Type.Int, Int64.to_int x)
    | Rpc.String x -> Ex (Type.String, x)
    | _ -> failwith "Value.t0_of_rpc: Protocol error."
end

module Bool_compare = struct type t = bool let compare = compare end
module Bool_set = Prime_enumset.Make_monadic (Bool_compare) (Lwt) (* TODO:opt *)
module Int_compare = struct type t = int let compare = compare end
module Int_set = Prime_enumset.Make_monadic (Int_compare) (Lwt)
module String_set = Prime_enumset.Make_monadic (String) (Lwt)

module Values = struct

  type 'a t =
    T : (module Prime_enumset.S with type elt = 'a and type t = 'b) * 'b -> 'a t

  let impl : type a. a Type.t1 -> (module Prime_enumset.S with type elt = a) =
    function
    | Type.Bool ->   (module Bool_set)
    | Type.Int ->    (module Int_set)
    | Type.String -> (module String_set)

  let empty : type a. a Type.t1 -> a t = fun t ->
    let module S = (val impl t) in
    T ((module S), S.empty)

  let is_empty (type a) (T ((module S), s) : a t) = S.is_empty s
  let contains (type a) (x : a) (T ((module S), s) : a t) = S.contains x s
  let locate (type a) (x : a) (T ((module S), s) : a t) = S.locate x s
  let min_elt (type a) (T ((module S), s) : a t) = S.min_elt s
  let max_elt (type a) (T ((module S), s) : a t) = S.max_elt s
  let add (type a) (x : a) (T ((module S), s) : a t) = T ((module S), S.add x s)
  let remove (type a) x (T ((module S), s) : a t) = T ((module S), S.remove x s)
  let iter (type a) f (T ((module S), s) : a t) = S.iter f s
  let fold (type a) f (T ((module S), s) : a t) acc = S.fold f s acc
  let for_all (type a) f (T ((module S), s) : a t) = S.for_all f s
  let exists (type a) f (T ((module S), s) : a t) = S.exists f s
  let filter (type a) f (T ((module S), s) : a t) = T ((module S), S.filter f s)
  let union (type a) (T ((module SA), sA) : a t) (T ((module SB), sB) : a t) =
    T ((module SB), SA.fold SB.add sA sB)
  let inter (type a) (T ((module SA), sA) : a t) (T ((module SB), sB) : a t) =
    T ((module SB), SB.filter (fun x -> SA.contains x sA) sB)
  let elements (type a) (T ((module S), s) : a t) = S.elements s

  let of_ordered_elements : type a. a Type.t1 -> a list -> a t = fun t s ->
    let module S = (val impl t) in
    T ((module S), S.of_ordered_elements s)
end

module Int32_set = Prime_enumset.Make_monadic (Int32) (Lwt)
module Int32_map = Prime_enummap.Make_monadic (Int32) (Lwt)
