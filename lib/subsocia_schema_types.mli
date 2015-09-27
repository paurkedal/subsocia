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

(** Schema type definitions. *)

open Subsocia_common
open Subsocia_selector_types

type et_allow =
  [ `Aux_string of string * string
  | `Allow_inclusion of string * Multiplicity.t * Multiplicity.t
  | `Allow_attribution of string * string ]

type et_adjust =
  [ et_allow
  | `Disallow_inclusion of string
  | `Disallow_attribution of string * string ]

type schema_add =
  [ `Aux_selector of string * selector
  | `Add_sub of selector
  | `Add_attr of selector ]

type schema_mod =
  [ schema_add
  | `Remove_sub of selector
  | `Remove_attr of selector
  | `Set_attr of selector ]

type schema_entry =
  [ `At_create of string * string
  | `At_delete of string
  | `Au_force of string list
  | `Au_relax of string list
  | `Et_create of string * et_allow list
  | `Et_modify of string * et_adjust list
  | `Et_delete of string
  | `Et_allow_dsub of string * string
  | `Et_disallow_dsub of string * string
  | `Et_allow_attribution of string * string * string
  | `Et_disallow_attribution of string * string * string
  | `Et_display of string * string
  | `E_create of selector * string
  | `E_force_dsub of selector * selector
  | `E_relax_dsub of selector * selector
  | `E_add_value of string * string * selector * selector
  | `E_remove_value of string * string * selector * selector
  | `Create of string * schema_add list
  | `Modify of selector * schema_mod list
  | `Delete of selector ]

type schema = schema_entry list
