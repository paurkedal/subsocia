(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(* TODO: Plugins should register themselves. *)

let loaded_plugins = Hashtbl.create 7

let load_plugin pkg =
  Lwt_log.ign_debug_f "Loading plugin %s." pkg;
  let aux () =
    if Hashtbl.mem loaded_plugins pkg then Some () else
    (Hashtbl.add loaded_plugins pkg (); None) in
  Caqti1_plugin.ensure_plugin aux pkg

let load_base_plugins () =
  List.iter load_plugin Subsocia_config.plugins#get

let load_cmd_plugins () =
  load_base_plugins ();
  List.iter load_plugin Subsocia_config.Cmd.plugins#get

let load_web_plugins () =
  load_base_plugins ();
  List.iter load_plugin Subsocia_config.Web.plugins#get
