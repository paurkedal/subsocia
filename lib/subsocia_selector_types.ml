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

(** Selector type definition. *)

type 'a adjacency = Dsub | Dsuper | Asub of 'a | Asuper of 'a

type attribute_predicate =
  | Attribute_present of string
  | Attribute_eq of string * string
  | Attribute_leq of string * string
  | Attribute_geq of string * string

type selector =
  | Select_with of selector * selector
  | Select_union of selector * selector
  | Select_inter of selector * selector
  | Select_adjacent of attribute_predicate adjacency
  | Select_type of string
  | Select_root
  | Select_id of int32
