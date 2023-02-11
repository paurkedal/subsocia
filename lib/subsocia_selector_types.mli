(* Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

(** Selector type definition. *)

open Subsocia_common

type attribute_selector =
  | Neighbour
  | Attribute_present of string
  | Attribute_eq of string * string
  | Attribute_leq of string * string
  | Attribute_geq of string * string

type selector =
  | Select_with of selector * selector
  | Select_union of selector * selector
  | Select_inter of selector * selector
  | Select_dsub
  | Select_dsuper
  | Select_image of attribute_selector
  | Select_preimage of attribute_selector
  | Select_type of string
  | Select_root
  | Select_id of int32 [@ocaml.deprecated]

type add_selector = selector option * string list String_map.t
type delete_selector = selector option * string list option String_map.t
