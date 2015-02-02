(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

module Attribute_key = struct
  external of_name : string -> int32 * Type.t0 = ""
  external of_id : int32 -> string * Type.t0 = ""
end

module Entity_type = struct
  external of_name : string -> int32 option = ""
  external name : int32 -> string = ""
  external inclusion_preds :
    int32 -> (int32 * Multiplicity.t * Multiplicity.t) list = ""
  external inclusion_succs :
    int32 -> (int32 * Multiplicity.t * Multiplicity.t) list = ""
  external attribution :
    int32 -> int32 -> (int32 * Multiplicity.t) list = ""
(*
  external attribution_preds :
    int32 -> (int32 * (int32 * Multiplicity.t) list) list = ""
  external attribution_succs :
    int32 -> (int32 * (int32 * Multiplicity.t) list) list = ""
*)
end

module Entity = struct
  external create : viewer: int32 -> admin: int32 -> int32 -> int32 = ""
  external type_ : int32 -> int32 = ""
  external viewer : int32 -> int32 = ""
  external admin : int32 -> int32 = ""
  external minimums : unit -> int32 list = ""
  external maximums : unit -> int32 list = ""
  external preds : int32 -> int32 list = ""
  external succs : int32 -> int32 list = ""
  external precedes : int32 -> int32 -> bool = ""
  external fetch_attribute : int32 -> int32 -> int32 -> Value.t0 list = ""
  external store_attribute : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external constrain : int32 -> int32 -> unit = ""
  external unconstrain : int32 -> int32 -> unit = ""
end
