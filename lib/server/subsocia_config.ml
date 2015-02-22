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

let group = new Config_file.group

let plugins =
  new Config_file.list_cp Config_file.string_wrappers ~group
    ["plugins"] []
    "List of dynamic findlib libraries to load at startup."

let database_uri =
  new Config_file.string_cp ~group ["database_uri"] "postgresql:/"
    "Caqti URI for connecting to the database."

let display_name =
  new Config_file.list_cp Config_file.string_wrappers ~group
    ["interpretation"; "display_name"]
    [ "display_name[]"; "display_name";
      "proper_name[]"; "proper_name";
      "common_name[]"; "common_name";
      "name[]"; "name";
      "unique_name" ]
    "Attributes considered as display names. \
     Suffix stranslated attributes with []"

module Cmd = struct
  let plugins =
    new Config_file.list_cp Config_file.string_wrappers ~group
      ["cmd"; "plugins"] []
      "List of extra plugins loaded by the command-line utility."
end

module Web = struct
  let plugins =
    new Config_file.list_cp Config_file.string_wrappers ~group
      ["web"; "plugins"] []
      "List of extra plugins loaded by the web server."
  let auth_http_header =
    new Config_file.string_cp ~group ["web"; "auth_http_header"]
      "X-Proxy-REMOTE-USER"
      "HTTP header used to identify a logged-in user."
  let auth_group =
    new Config_file.string_cp ~group ["web"; "auth_group"]
      "Authenticated Users"
      "Authorization group to use for the web interface."
end

let () =
  try group#read (Sys.getenv "SUBSOCIA_CONFIG")
  with Not_found ->
    if Sys.file_exists "/etc/subsocia.conf" then
      group#read "/etc/subsocia.conf"
