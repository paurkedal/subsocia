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

open Cmdliner
open Subsocia_cmdliner

open Command_common
open Command_db
open Command_et
open Command_in
open Command_at
open Command_an
open Command_au
open Command_e

(* Entities *)

let load schema_path disable_transaction =
  let schema = Subsocia_schema.load schema_path in
  run_exn @@ fun (module C) ->
  if disable_transaction then
    let module Schema = Subsocia_schema.Make (C) in
    Schema.exec schema
  else
    C.transaction @@
      (fun (module C) ->
        let module Schema = Subsocia_schema.Make (C) in
        Schema.exec schema)

let load_cmd =
  let schema =
    Arg.(required & pos 0 (some file) None & info ~docv:"PATH" [])
  in
  let term = Term.(const load $ schema $ Arg.disable_transaction) in
  let info =
    let doc =
      "Add, modify, and delete attributes according to the schema loaded \
       from PATH."
    in
    Cmd.info ~doc "load"
  in
  Cmd.v info (with_log term)

(* Main *)

let cmds = [
  db_schema_cmd;
  db_init_cmd;
  db_upgrade_cmd;

  et_info_cmd;
  et_list_cmd;
  et_create_cmd;
  et_modify_cmd;
  et_delete_cmd;

  in_allow_cmd;
  in_disallow_cmd;
  in_list_cmd;

  at_create_cmd;
  at_delete_cmd;
  at_list_cmd;

  au_force_cmd;
  au_relax_cmd;
  au_list_cmd;

  an_allow_cmd;
  an_disallow_cmd;
  an_list_cmd;

  e_ls_cmd;
  e_search_cmd;
  e_fts_cmd;
  e_create_cmd;
  e_delete_cmd;
  e_modify_cmd;

  load_cmd;
]

let main_cmd =
  let info = Cmd.info ?version:Subsocia_version.pkg_version "subsocia" in
  Cmd.group info cmds

let () =
  Dynlink.allow_unsafe_modules true;
  exit (Cmdliner.Cmd.eval' main_cmd)
