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

open Subsocia_selector_types

type schema_add =
  [ `Add_sub of selector
  | `Add_attr of selector ]

type schema_mod =
  [ schema_add
  | `Remove_sub of selector
  | `Remove_attr of selector
  | `Set_attr of selector ]

type schema_entry =
  [ `Create of string * schema_add list
  | `Modify of selector * schema_mod list
  | `Delete of selector ]

type schema = schema_entry list
