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

type encoded_attribute_predicate =
  | Eap_inter of encoded_attribute_predicate list
  | Eap_present of int32
  | Eap_eq of int32 * Value.ex
  | Eap_in of int32 * Values.ex
  | Eap_leq of int32 * Value.ex
  | Eap_geq of int32 * Value.ex
  | Eap_between of int32 * Value.ex * Value.ex
  | Eap_search of int32 * string
  | Eap_search_fts of string
  with rpc
