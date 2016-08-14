(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
  external of_name : string -> (int32 * Type.ex * Multiplicity.t) option = ""
  external of_soid : int32 -> string * Type.ex * Multiplicity.t = ""
  external create : Type.ex -> Multiplicity.t -> string -> int32 = ""
  external delete : int32 -> unit = ""
  external all : unit -> int32 list = ""
end

module Attribute_uniqueness = struct
  external force : int32 list -> int32 = ""
  external relax : int32 -> unit = ""
  external find : int32 list -> int32 option = ""
  external all : unit -> int32 list = ""
  external affecting : int32 -> int32 list = ""
  external affected : int32 -> int32 list = ""
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
  external can_attribute : int32 -> int32 -> int32 -> bool = ""
  external allowed_attributes : int32 -> int32 -> int32 list = ""
  external allowed_mappings : int32 -> (int32 * int32) list = ""
  external allowed_attributions : unit -> (int32 * int32 * int32) list = ""
  external allow_attribution : int32 -> int32 -> int32 -> unit = ""
  external disallow_attribution : int32 -> int32 -> int32 -> unit = ""
end

module Entity = struct
  external create : int32 -> int32 = ""
  external delete : int32 -> unit = ""
  external entity_type : int32 -> int32 = ""
  external rank : int32 -> int = ""
  external type_members : int32 -> int32 list = ""
  external root : unit -> int32 = ""
  external minimums : unit -> int32 list = ""
  external dsub : int32 option -> int32 -> int32 list = ""
  external dsuper : int32 option -> int32 -> int32 list = ""
  external is_dsub : int32 -> int32 -> bool = ""
  external is_sub : int32 -> int32 -> bool = ""
  external get_values : int32 -> int32 -> int32 -> Value.ex list = ""
  external add_values : int32 -> Value.ex list ->
                        int32 -> int32 -> int32 list = ""
  external remove_values : int32 -> Value.ex list ->
                           int32 -> int32 -> unit = ""
  external set_values : int32 -> Value.ex list ->
                            int32 -> int32 -> int32 list = ""
  external image1 : encoded_attribute_predicate -> int32 -> int32 list = ""
  external preimage1 : encoded_attribute_predicate -> int32 -> int32 list = ""
  external image1_eq : int32 -> Value.ex -> int32 -> int32 list = ""
  external preimage1_eq : int32 -> Value.ex -> int32 -> int32 list = ""
  external image1_fts :
    ?entity_type: int32 -> ?super: int32 -> ?cutoff: float -> ?limit: int ->
    string -> int32 -> (int32 * float) list = ""
  external preimage1_fts :
    ?entity_type: int32 -> ?super: int32 -> ?cutoff: float -> ?limit: int ->
    string -> int32 -> (int32 * float) list = ""
  external mapping1 : int32 -> int32 -> (int32 * Value.ex) list = ""
  external premapping1 : int32 -> int32 -> (int32 * Value.ex) list = ""
  external force_dsub : int32 -> int32 -> unit = ""
  external relax_dsub : int32 -> int32 -> unit = ""
end
