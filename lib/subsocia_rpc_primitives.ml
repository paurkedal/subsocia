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

module Attribute_type = struct
  external of_name : string -> (int32 * Type.t0) option = ""
  external of_id : int32 -> string * Type.t0 = ""
end

module Entity_type = struct
  external of_name : string -> int32 option = ""
  external name : int32 -> string = ""
  external create : string -> int32 = ""
  external delete : int32 -> unit = ""
  external all : unit -> int32 list = ""
  external inclusion_preds :
    int32 -> (int32 * Multiplicity.t * Multiplicity.t) list = ""
  external inclusion_succs :
    int32 -> (int32 * Multiplicity.t * Multiplicity.t) list = ""
  external inclusion_allow :
    Multiplicity.t -> Multiplicity.t -> int32 -> int32 -> unit = ""
  external inclusion_disallow : int32 -> int32 -> unit = ""
  external attribution_mult :
    int32 -> int32 -> int32 -> Multiplicity.t option = ""
  external attribution :
    int32 -> int32 -> (int32 * Multiplicity.t) list = ""
end

module Entity = struct
  external create : viewer: int32 -> admin: int32 -> int32 -> int32 = ""
  external type_ : int32 -> int32 = ""
  external viewer : int32 -> int32 = ""
  external admin : int32 -> int32 = ""
  external type_members : int32 -> int32 list = ""
  external minimums : unit -> int32 list = ""
  external maximums : unit -> int32 list = ""
  external preds : int32 -> int32 list = ""
  external succs : int32 -> int32 list = ""
  external precedes : int32 -> int32 -> bool = ""
  external getattr : int32 -> int32 -> int32 -> Value.t0 list = ""
  external setattr : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external addattr : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external delattr : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external apreds : int32 -> int32 -> Value.t0 -> int32 list = ""
  external asuccs : int32 -> int32 -> Value.t0 -> int32 list = ""
  external constrain : int32 -> int32 -> unit = ""
  external unconstrain : int32 -> int32 -> unit = ""
end
