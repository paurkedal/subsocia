(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Cmdliner
open Command_common
open Printf
open Subsocia_common
open Subsocia_version
open Unprime

let schema_dir =
  try Sys.getenv "SUBSOCIA_SCHEMA_DIR"
  with Not_found -> schema_dir

let db_schema do_dir =
  Lwt_main.run begin
    if do_dir then
      Lwt_io.printl schema_dir
    else
      Lwt_list.iter_s
        (fun schema ->
          Lwt_io.printl (Filename.concat schema_dir schema))
        (upgradable_sql_schemas @ idempotent_sql_schemas)
  end; 0

let db_schema_t =
  let do_dir_t =
    Arg.(value & flag &
         info ~doc:"Print the path to the top-level directory \
                    instead of to the individual schema files." ["dir"]) in
  Term.(pure db_schema $ do_dir_t)

let load_sql (module C : Caqti_lwt.CONNECTION) sql =
  Lwt_io.with_file ~mode:Lwt_io.input sql @@ fun ic ->
  let rec loop () =
    match%lwt Caqti_lwt_sql_io.read_sql_statement Lwt_io.read_char_opt ic with
    | None -> Lwt.return_unit
    | Some stmt -> C.exec (Caqti_query.oneshot_sql stmt) [||] >> loop () in
  loop ()

let db_init disable_transaction = run0 @@ fun (module C) ->
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  let%lwt cc = Caqti_lwt.connect uri in
  Lwt_list.iter_s
    (fun fn ->
      let fp = Filename.concat schema_dir fn in
      Lwt_log.info_f "Loading %s." fp >>
      load_sql cc fp)
    (upgradable_sql_schemas @ idempotent_sql_schemas) >>
  Lwt_list.iter_s
    (fun fn ->
      let fp = Filename.concat schema_dir fn in
      Lwt_log.info_f "Loading %s." fp >>
      let schema = Subsocia_schema.load fp in
      if disable_transaction then
        let module Schema = Subsocia_schema.Make (C) in
        Schema.exec schema
      else
        C.transaction @@
          (fun (module C) ->
            let module Schema = Subsocia_schema.Make (C) in
            Schema.exec schema))
    subsocia_schemas

let db_init_t = Term.(pure db_init $ disable_transaction_t)

let get_schema_version_q = Subsocia_direct.format_query
  "SELECT global_value FROM @global_integer \
   WHERE global_name = 'schema_version'"
let get_schema_version (module C : Caqti_lwt.CONNECTION) =
  C.find get_schema_version_q C.Tuple.(int 0) [||]

let string_of_query_info = function
  | `Oneshot s -> String.trim s
  | `Prepared (_, s) -> String.trim s

let db_upgrade () = Lwt_main.run begin
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  let%lwt c = Caqti_lwt.connect uri in
  let module C : Caqti_lwt.CONNECTION = (val c) in
  let%lwt db_schema_version = get_schema_version c in
  let have_error = ref false in
  let load fp =
    if !have_error then Lwt_io.printlf "Skipped: %s" fp else
    try%lwt
      load_sql c fp >>
      Lwt_io.printlf "Updated: %s" fp
    with
    | Caqti.Execute_failed (_, qi, msg) ->
      have_error := true;
      Lwt_io.printlf "Failed: %s" fp >>
      Lwt_io.printlf "<<- %s" (string_of_query_info qi) >>
      Lwt_io.printlf "->> %s" (String.trim msg)
    | exc ->
      have_error := true;
      Lwt_io.printlf "Failed: %s" fp >>
      Lwt_io.printlf "Exception: %s" (Printexc.to_string exc) in
  for%lwt v = db_schema_version to schema_version - 1 do
    load (Filename.concat schema_upgrade_dir (sprintf "from-%d.sql" v))
  done >>
  Lwt_list.iter_s (load <@ Filename.concat schema_dir)
                  idempotent_sql_schemas >>
  if !have_error then
    Lwt_io.printf "\n\
      You may need to inspect the database and schema and apply the failed\n\
      update manually.  Make sure also include the the update of the\n\
      'schema_version' in the global_integer table.\n\n\
      If this looks like a bug, please file an issue at\n%s.\n"
      issues_url >>
    Lwt.return 69
  else
    Lwt_io.printlf "All updates succeeded." >>
    Lwt.return 0
end

let db_upgrade_t = Term.(pure db_upgrade $ pure ())
