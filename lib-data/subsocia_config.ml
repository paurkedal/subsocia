(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

let group = new Config_file.group

let plugins =
  new Config_file.list_cp Config_file.string_wrappers ~group
    ["plugins"] []
    "List of dynamic findlib libraries to load at startup."

let database_uri =
  new Config_file.string_cp ~group ["database_uri"] "postgresql://"
    "Caqti URI for connecting to the database."

let enable_caching =
  new Config_file.bool_cp ~group ["enable_caching"] true
    "Set to false to disable memory caching."

module Cmd = struct
  let plugins =
    new Config_file.list_cp Config_file.string_wrappers ~group
      ["cmd"; "plugins"] []
      "List of extra plugins loaded by the command-line utility."
end

let () =
  try group#read (Sys.getenv "SUBSOCIA_CONFIG")
  with Not_found ->
    if Sys.file_exists "/etc/subsocia.conf" then
      group#read "/etc/subsocia.conf"
