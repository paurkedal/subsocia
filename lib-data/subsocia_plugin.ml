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

(* TODO: Plugins should register themselves. *)

let debug =
  try bool_of_string (Sys.getenv "SUBSOCIA_DEBUG_DYNLOAD")
  with Not_found -> false

let backend_predicates () =
  (match Sys.backend_type with
   | Sys.Native -> ["native"]
   | Sys.Bytecode -> ["byte"]
   | Sys.Other _ -> [])

let assume_linked = [
  "subsocia.data";
]

let init = lazy begin
  let predicates = backend_predicates () in
  Findlib.record_package_predicates predicates;
  List.iter (Findlib.record_package Findlib.Record_core)
            (Findlib.package_deep_ancestors predicates assume_linked);
end

let load_base_plugins () =
  Lazy.force init;
  Fl_dynload.load_packages ~debug Subsocia_config.(global.plugins)

let load_cmd_plugins () =
  load_base_plugins ();
  Fl_dynload.load_packages ~debug Subsocia_config.(global.cmd_plugins)
