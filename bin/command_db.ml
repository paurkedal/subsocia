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
open Command_common
open Lwt.Infix
open Lwt.Syntax
open Printf
open Subsocia_cmdliner
open Subsocia_version
open Unprime

let docs = "DATABASE COMMANDS"

let ( let*? ) = Lwt_result.Syntax.( let* )

let show_error = function
 | #Caqti_error.t as err -> Caqti_error.show err
 | `Msg msg -> msg

let or_fail = function
 | Ok x -> Lwt.return x
 | Error #Caqti_error.t as r -> Caqti_lwt.or_fail r
 | Error (`Msg msg) -> Lwt.fail_with msg

let env =
  let schema =
    (match Subsocia_connection.db_schema with
     | None -> Caqti_query.S []
     | Some schema_name -> Caqti_query.L schema_name)
  in
  fun _ -> function
   | "" -> schema
   | "dollar" -> Caqti_query.L "$"
   | _ -> raise Not_found

let schema_dir =
  try Sys.getenv "SUBSOCIA_SCHEMA_DIR"
  with Not_found -> schema_dir

let all_schemas =
  sql_schemas.step1_idempotent @
  sql_schemas.step2_upgradable @
  sql_schemas.step3_idempotent

let db_schema do_dir =
  Lwt_main.run begin
    if do_dir then
      Lwt_io.printl schema_dir
    else
      Lwt_list.iter_s
        (fun schema -> Lwt_io.printl (Filename.concat schema_dir schema))
        all_schemas
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
  Cmd.v info (with_log term)

let angstrom_file_parser =
  let open Angstrom in
  let is_space = function
   | ' ' | '\t' | '\n' | '\r' -> true
   | _ -> false
  in
  let white =
    many (take_while1 is_space <|> (string "--" *> take_till ((=) '\n')))
      <* commit
  in
  white *> many (Caqti_query.angstrom_parser <* char ';' <* white)

let load_sql (module C : Caqti_lwt.CONNECTION) sql =
  let open Caqti_request.Infix in
  Lwt_io.with_file ~mode:Lwt_io.input sql @@ fun ic ->
  let* unconsumed, result = Angstrom_lwt_unix.parse angstrom_file_parser ic in
  if unconsumed.Angstrom.Buffered.len <> 0 then
    Lwt.return_error (`Msg (sql ^ ": Unconsumed input."))
  else
  let rec submit = function
   | [] -> Lwt.return_ok ()
   | stmt :: stmts ->
      let request = Caqti_type.(unit -->. unit) ~oneshot:true (fun _ -> stmt) in
      let*? () = C.exec request () in
      submit stmts
  in
  (match result with
   | Error msg -> Lwt.return_error (`Msg (sql ^ ": " ^ msg))
   | Ok stmts -> submit stmts)

let db_init disable_transaction = Lwt_main.run begin
  let uri = Subsocia_connection.db_uri in
  let* cc = Caqti_lwt.connect ~env uri >>= Caqti_lwt.or_fail in
  let module Cc = (val cc) in
  let* () =
    (match Subsocia_connection.db_schema with
     | None -> Lwt.return_unit
     | Some schema_name ->
        let create_schema_request =
          let q = Caqti_query.(S[L"CREATE SCHEMA "; L schema_name]) in
          let open Caqti_request.Infix in
          let open Caqti_type.Std in
          (unit -->. unit) ~oneshot:true (fun _ -> q)
        in
        Cc.exec create_schema_request () >>= Caqti_lwt.or_fail)
  in
  let* () =
    Lwt_list.iter_s
      (fun fn ->
        let fp = Filename.concat schema_dir fn in
        Log.info (fun f -> f "Loading %s." fp) >>= fun () ->
        load_sql cc fp >>= or_fail)
      all_schemas
  in
  let* () = Cc.disconnect () in
  let module Sc = (val connect ()) in
  Lwt_list.iter_s
    (fun fn ->
      let fp = Filename.concat schema_dir fn in
      Log.info (fun f -> f "Loading %s." fp) >>= fun () ->
      let schema = Subsocia_schema.load fp in
      if disable_transaction then
        let module Schema = Subsocia_schema.Make (Sc) in
        Schema.exec schema
      else
        Sc.transaction @@
          (fun (module Sc) ->
            let module Schema = Subsocia_schema.Make (Sc) in
            Schema.exec schema))
    subsocia_schemas >|= fun () ->
  0
end

let db_init_cmd =
  let term = Term.(const db_init $ Arg.disable_transaction) in
  let info = Cmd.info ~docs ~doc:"Initialize the database." "db-init" in
  Cmd.v info (with_log term)

let get_schema_version_q =
  let open Caqti_request.Infix in
  Caqti_type.(unit ->! int)
    "SELECT global_value FROM $.global_integer \
     WHERE global_name = 'schema_version'"
let get_schema_version (module C : Caqti_lwt.CONNECTION) =
  C.find get_schema_version_q ()

let db_upgrade () = Lwt_main.run begin
  let uri = Subsocia_connection.db_uri in
  let* c = Caqti_lwt.connect ~env uri >>= Caqti_lwt.or_fail in
  let module C : Caqti_lwt.CONNECTION = (val c) in
  let* db_schema_version = get_schema_version c >>= Caqti_lwt.or_fail in
  let have_error = ref false in
  let load fp =
    if !have_error then Lwt_io.printlf "Skipped: %s" fp else
    (load_sql c fp >>= function
     | Ok () ->
        Lwt_io.printlf "Updated: %s" fp
     | Error err ->
        have_error := true;
        Lwt_io.printlf "Failed: %s" fp >>= fun () ->
        Lwt_io.printlf "Error: %s" (show_error err))
  in
  let* () =
    Lwt_list.iter_s
      (load % Filename.concat schema_dir)
      sql_schemas.step1_idempotent
  in
  let* () =
    for%lwt v = db_schema_version to schema_version - 1 do
      load (Filename.concat schema_upgrade_dir (sprintf "from-%d.sql" v))
    done
  in
  let* () =
    Lwt_list.iter_s
      (load % Filename.concat schema_dir)
      sql_schemas.step3_idempotent
  in
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
  Cmd.v info (with_log term)
