(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Infix
open Printf
open Subsocia_version
open Unprime

let (>>=??) m f = m >>= function Ok x -> f x | Error _ as r -> Lwt.return r

let env _ = function
 | "." -> Caqti_request.L !Subsocia_direct.schema_prefix
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
    Arg.(value & flag &
         info ~doc:"Print the path to the top-level directory \
                    instead of to the individual schema files." ["dir"]) in
  Term.(const db_schema $ do_dir_t)

let load_sql (module C : Caqti_lwt.CONNECTION) sql =
  Lwt_io.with_file ~mode:Lwt_io.input sql @@ fun ic ->
  let rec loop () =
    (match%lwt Caqti_lwt_sql_io.read_sql_statement Lwt_io.read_char_opt ic with
     | None ->
        Lwt.return_ok ()
     | Some stmt ->
        C.exec (Caqti_request.exec ~oneshot:true Caqti_type.unit stmt) ()
        >>=?? loop) in
  loop ()

let db_init disable_transaction = Lwt_main.run begin
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  let%lwt cc = Caqti_lwt.connect uri >>= Caqti_lwt.or_fail in
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

let db_init_cmd = Term.(const db_init $ disable_transaction_t)

let get_schema_version_q =
  Caqti_request.find ~env Caqti_type.unit Caqti_type.int
    "SELECT global_value FROM $.global_integer \
     WHERE global_name = 'schema_version'"
let get_schema_version (module C : Caqti_lwt.CONNECTION) =
  C.find get_schema_version_q ()

let db_upgrade () = Lwt_main.run begin
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  let%lwt c = Caqti_lwt.connect uri >>= Caqti_lwt.or_fail in
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
        Lwt_io.printlf "Error: %s" (Caqti_error.show err)) in
  for%lwt v = db_schema_version to schema_version - 1 do
    load (Filename.concat schema_upgrade_dir (sprintf "from-%d.sql" v))
  done >>= fun () ->
  Lwt_list.iter_s (load % Filename.concat schema_dir)
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

let db_upgrade_cmd = Term.(const db_upgrade $ const ())
