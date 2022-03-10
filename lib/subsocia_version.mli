(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Information about the package and installation. *)

val pkg_version : string option
val pkg_hash : string option

val schema_version : int
val schema_dir : string
val schema_upgrade_dir : string

type sql_schemas = {
  step1_idempotent: string list;
  step2_upgradable: string list;
  step3_idempotent: string list;
}
val sql_schemas : sql_schemas

val subsocia_schemas : string list

val issues_url : string
