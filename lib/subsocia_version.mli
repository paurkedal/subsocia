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

(** Information about the package and installation. *)

val pkg_version : string
val pkg_datadir : string

val schema_version : int
val schema_dir : string
val schema_upgrade_dir : string

val upgradable_sql_schemas : string list
val idempotent_sql_schemas : string list
val subsocia_schemas : string list

val static_dir : string

val issues_url : string
