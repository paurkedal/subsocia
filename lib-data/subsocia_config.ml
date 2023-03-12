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

type t = {
  database_uri: string [@default "postgresql://"];
  (** Caqti URI for connecting to the database. *)

  enable_caching: bool [@default true];
  (** Set to false to disable memory caching. *)

  plugins: string list [@default []];
  (** List of dynamic findlib libraries to load at startup. *)

  cmd_plugins: string list [@default []];
  (** List of extra plugins loaded by the command-line utility. *)
}
[@@deriving yojson]

let global =
  let config =
    try Sys.getenv "SUBSOCIA_CONFIG" with Not_found -> "/etc/subsocia.conf"
  in
  (match of_yojson (Yojson.Safe.from_file config) with
   | Ok g -> g
   | Error msg -> Printf.ksprintf failwith "Cannot load %s: %s." config msg)
