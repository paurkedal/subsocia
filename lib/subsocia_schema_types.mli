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

(** Schema type definitions. *)

open Subsocia_common
open Subsocia_selector_types

type et_allow =
  [ `Allow_inclusion of string * Multiplicity.t * Multiplicity.t
  | `Allow_attribution of string * string * Multiplicity.t ]

type et_adjust =
  [ et_allow
  | `Disallow_inclusion of string
  | `Disallow_attribution of string * string ]

type schema_add =
  [ `Add_sub of selector
  | `Add_attr of selector ]

type schema_mod =
  [ schema_add
  | `Remove_sub of selector
  | `Remove_attr of selector
  | `Set_attr of selector ]

type schema_entry =
  [ `At_create of string * string
  | `At_delete of string
  | `Et_create of string * et_allow list
  | `Et_modify of string * et_adjust list
  | `Et_delete of string
  | `Create of string * schema_add list
  | `Modify of selector * schema_mod list
  | `Delete of selector ]

type schema = schema_entry list
