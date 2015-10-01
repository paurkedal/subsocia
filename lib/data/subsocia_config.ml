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

module Web = struct
  let plugins =
    new Config_file.list_cp Config_file.string_wrappers ~group
      ["web"; "plugins"] []
      "List of extra plugins loaded by the web server."
  let auth_top =
    new Config_file.string_cp ~group
      ["web"; "auth_top"] "auth"
      "Group under which authentication methods are placed."
  let hba_method_header =
    new Config_file.option_cp Config_file.string_wrappers ~group
      ["web"; "hba_method_header"] None
      "HTTP header containing the authentication method if not fixed."
  let hba_method =
    new Config_file.option_cp Config_file.string_wrappers ~group
      ["web"; "hba_method"] (Some "default")
      "Authentication method if hba_method_header is unset or not the \
       header is missing."
  let hba_identity_header =
    new Config_file.option_cp Config_file.string_wrappers ~group
      ["web"; "hba_identity_header"] (Some "Host")
      "HTTP header containing the identity of an authenticated user. \
       If unset, disables header-based authentication."
  let restapi_allowed_attributes =
    new Config_file.list_cp Config_file.string_wrappers ~group
      ["web"; "restapi_allowed_attributes"] []
      "Attributes which are exposed by the REST API. This must be \
       valid JSON identifiers due to the way they are returned."

  let completion_cutoff =
    new Config_file.float_cp ~group
      ["web"; "completion_cutoff"] 0.001
      "Full-text search score cut-off to use in web interface."
  let completion_limit =
    new Config_file.int_cp ~group
      ["web"; "completion_limit"] 10
      "Maximum number of results to return for full-text search in web \
       interface."
end

let () =
  try group#read (Sys.getenv "SUBSOCIA_CONFIG")
  with Not_found ->
    if Sys.file_exists "/etc/subsocia.conf" then
      group#read "/etc/subsocia.conf"
