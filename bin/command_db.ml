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

open Cmdliner
open Command_common
open Lwt.Infix
open Printf
open Subsocia_cmdliner
open Subsocia_version
open Unprime

let docs = "DATABASE COMMANDS"

let (>>=??) m f = m >>= function Ok x -> f x | Error _ as r -> Lwt.return r

let db_schema_prefix =
  (match Subsocia_connection.db_schema with
   | None -> ""
   | Some s -> s ^ ".")

let env =
  fun _ -> function
   | "." -> Caqti_query.L db_schema_prefix
   | _ -> raise Not_found

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

let db_schema_cmd =
  let do_dir_t =
    let doc =
      "Print the path to the top-level directory instead of to the individual \
       schema files."
    in
    Arg.(value & flag & info ~doc ["dir"])
  in
  let term = Term.(const db_schema $ do_dir_t) in
  let info =
    let doc = "Print the directory or paths of database schema files." in
    Cmd.info ~docs ~doc "db-schema"
  in
  Cmd.v info term

let subsocia_dot_re = Re.compile Re.(seq [bow; str "subsocia."])

let load_sql (module C : Caqti_lwt.CONNECTION) sql =
  let open Caqti_request.Infix in
  Lwt_io.with_file ~mode:Lwt_io.input sql @@ fun ic ->
  let rec loop () =
    (match%lwt Caqti_lwt_sql_io.read_sql_statement Lwt_io.read_char_opt ic with
     | None ->
        Lwt.return_ok ()
     | Some "CREATE SCHEMA subsocia" ->
        (match Subsocia_connection.db_schema with
         | None -> loop ()
         | Some schema ->
            let stmt = "CREATE SCHEMA " ^ schema in
            C.exec (Caqti_type.(unit ->. unit) ~oneshot:true stmt) ()
            >>=?? loop)
     | Some stmt ->
        let stmt =
          if db_schema_prefix = "subsocia." then stmt else
          Re.replace_string subsocia_dot_re ~by:db_schema_prefix stmt in
        C.exec (Caqti_type.(unit ->. unit) ~oneshot:true stmt) ()
        >>=?? loop)
  in
  loop ()

let db_init disable_transaction = Lwt_main.run begin
  let uri = Subsocia_connection.db_uri in
  let%lwt cc = Caqti_lwt.connect ~env uri >>= Caqti_lwt.or_fail in
  Lwt_list.iter_s
    (fun fn ->
      let fp = Filename.concat schema_dir fn in
      Lwt_log.info_f "Loading %s." fp >>= fun () ->
      load_sql cc fp >>= Caqti_lwt.or_fail)
    (upgradable_sql_schemas @ idempotent_sql_schemas) >>= fun () ->
  (let module C = (val cc) in C.disconnect ()) >>= fun () ->
  let module C = (val connect ()) in
  Lwt_list.iter_s
    (fun fn ->
      let fp = Filename.concat schema_dir fn in
      Lwt_log.info_f "Loading %s." fp >>= fun () ->
      let schema = Subsocia_schema.load fp in
      if disable_transaction then
        let module Schema = Subsocia_schema.Make (C) in
        Schema.exec schema
      else
        C.transaction @@
          (fun (module C) ->
            let module Schema = Subsocia_schema.Make (C) in
            Schema.exec schema))
    subsocia_schemas >|= fun () ->
  0
end

let db_init_cmd =
  let term = Term.(const db_init $ Arg.disable_transaction) in
  let info = Cmd.info ~docs ~doc:"Initialize the database." "db-init" in
  Cmd.v info term

let get_schema_version_q =
  let open Caqti_request.Infix in
  Caqti_type.(unit ->! int)
    "SELECT global_value FROM $.global_integer \
     WHERE global_name = 'schema_version'"
let get_schema_version (module C : Caqti_lwt.CONNECTION) =
  C.find get_schema_version_q ()

let db_upgrade () = Lwt_main.run begin
  let uri = Subsocia_connection.db_uri in
  let%lwt c = Caqti_lwt.connect ~env uri >>= Caqti_lwt.or_fail in
  let module C : Caqti_lwt.CONNECTION = (val c) in
  let%lwt db_schema_version = get_schema_version c >>= Caqti_lwt.or_fail in
  let have_error = ref false in
  let load fp =
    if !have_error then Lwt_io.printlf "Skipped: %s" fp else
    (match%lwt load_sql c fp with
     | Ok () ->
        Lwt_io.printlf "Updated: %s" fp
     | Error err ->
        have_error := true;
        Lwt_io.printlf "Failed: %s" fp >>= fun () ->
        Lwt_io.printlf "Error: %s" (Caqti_error.show err))
  in
  for%lwt v = db_schema_version to schema_version - 1 do
    load (Filename.concat schema_upgrade_dir (sprintf "from-%d.sql" v))
  done >>= fun () ->
  Lwt_list.iter_s
    (load % Filename.concat schema_dir)
    idempotent_sql_schemas >>= fun () ->
  if !have_error then
    Lwt_io.printf "\n\
      You may need to inspect the database and schema and apply the failed\n\
      update manually.  Make sure also include the the update of the\n\
      'schema_version' in the global_integer table.\n\n\
      If this looks like a bug, please file an issue at\n%s.\n"
      issues_url >>= fun () ->
    Lwt.return 69
  else
    Lwt_io.printlf "All updates succeeded." >>= fun () ->
    Lwt.return 0
end

let db_upgrade_cmd =
  let term = Term.(const db_upgrade $ const ()) in
  let info =
    let doc = sprintf
      "Upgrade the database to the current schema version (%d)."
      schema_version
    in
    Cmd.info ~docs ~doc "db-upgrade"
  in
  Cmd.v info term
