(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
open Subsocia_rpc_types

module Attribute_type = struct
  external of_name : string -> (int32 * Type.t0) option = ""
  external of_id : int32 -> string * Type.t0 = ""
  external create : Type.t0 -> string -> int32 = ""
  external delete : int32 -> unit = ""
end

module Entity_type = struct
  external of_name : string -> int32 option = ""
  external name : int32 -> string = ""
  external create : string -> int32 = ""
  external delete : int32 -> unit = ""
  external all : unit -> int32 list = ""
  external entity_name_tmpl : int32 -> string = ""
  external set_entity_name_tmpl : int32 -> string -> unit = ""
  external can_dsub :
    int32 -> int32 -> (Multiplicity.t * Multiplicity.t) option = ""
  external dsub :
    int32 -> (int32 * Multiplicity.t * Multiplicity.t) list = ""
  external dsuper :
    int32 -> (int32 * Multiplicity.t * Multiplicity.t) list = ""
  external dsub_elements :
    unit -> (int32 * int32 * Multiplicity.t * Multiplicity.t) list = ""
  external allow_dsub :
    Multiplicity.t -> Multiplicity.t -> int32 -> int32 -> unit = ""
  external disallow_dsub : int32 -> int32 -> unit = ""
  external can_asub :
    int32 -> int32 -> int32 -> Multiplicity.t option = ""
  external can_asub_byattr :
    int32 -> int32 -> (int32 * Multiplicity.t) list = ""
  external asub_elements :
    unit -> (int32 * int32 * int32 * Multiplicity.t) list = ""
  external allow_asub :
    int32 -> int32 -> int32 -> Multiplicity.t -> unit = ""
  external disallow_asub :
    int32 -> int32 -> int32 -> unit = ""
end

module Entity = struct
  external create : ?access: int32 -> int32 -> int32 = ""
  external modify : ?access: int32 -> int32 -> unit = ""
  external delete : int32 -> unit = ""
  external type_ : int32 -> int32 = ""
  external rank : int32 -> int = ""
  external access : int32 -> int32 = ""
  external type_members : int32 -> int32 list = ""
  external top : unit -> int32 = ""
  external minimums : unit -> int32 list = ""
  external dsub : int32 option -> int32 -> int32 list = ""
  external dsuper : int32 option -> int32 -> int32 list = ""
  external is_sub : int32 -> int32 -> bool = ""
  external getattr : int32 -> int32 -> int32 -> Value.t0 list = ""
  external setattr : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external addattr : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external delattr : int32 -> int32 -> int32 -> Value.t0 list -> unit=""
  external asub : int32 -> encoded_attribute_predicate -> int32 list = ""
  external asuper : int32 -> encoded_attribute_predicate -> int32 list = ""
  external asub_eq : int32 -> int32 -> Value.t0 -> int32 list = ""
  external asuper_eq : int32 -> int32 -> Value.t0 -> int32 list = ""
  external asub_fts : ?entity_type: int32 -> ?cutoff: float -> ?limit: int ->
		      int32 -> string -> (int32 * float) list = ""
  external asuper_fts : ?entity_type: int32 -> ?cutoff: float -> ?limit: int ->
			int32 -> string -> (int32 * float) list = ""
  external asub_get : int32 -> int32 -> (int32 * Value.t0) list = ""
  external asuper_get : int32 -> int32 -> (int32 * Value.t0) list = ""
  external force_dsub : int32 -> int32 -> unit = ""
  external relax_dsub : int32 -> int32 -> unit = ""
end
