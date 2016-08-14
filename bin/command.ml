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
open Printf
open Subsocia_version

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
  run0 @@ fun (module C) ->
  if disable_transaction then
    let module Schema = Subsocia_schema.Make (C) in
    Schema.exec schema
  else
    C.transaction @@
      (fun (module C) ->
        let module Schema = Subsocia_schema.Make (C) in
        Schema.exec schema)

let load_t =
  let schema_t = Arg.(required & pos 0 (some file) None &
                      info ~docv:"PATH" []) in
  Term.(pure load $ schema_t $ disable_transaction_t)

(* Main *)

let db_scn = "DATABASE COMMANDS"
let et_scn = "ENTITY TYPE COMMANDS"
let in_scn = "INCLUSION COMMANDS"
let at_scn = "ATTRIBUTE TYPE COMMANDS"
let au_scn = "ATTRIBUTE UNIQUENESS"
let an_scn = "ATTRIBUTION COMMANDS"
let e_scn = "ENTITY COMMANDS"

let subcommands = [
  db_schema_t, Term.info ~docs:db_scn
    ~doc:"Print the directory or paths of database schema files."
    "db-schema";
  db_init_t, Term.info ~docs:db_scn
    ~doc:"Initialize the database."
    "db-init";
  db_upgrade_t, Term.info ~docs:db_scn
    ~doc:(sprintf "Upgrade the database to the current schema version (%d)."
                  schema_version)
    "db-upgrade";

  et_list_t, Term.info ~docs:et_scn
    ~doc:"List entity types."
    "et-list";
  et_create_t, Term.info ~docs:et_scn
    ~doc:"Create an entity type."
    "et-create";
  et_modify_t, Term.info ~docs:et_scn
    ~doc:"Modify an entity type."
    "et-modify";
  et_delete_t, Term.info ~docs:et_scn
    ~doc:"Delete an entity type."
    "et-delete";

  in_allow_t, Term.info ~docs:in_scn
    ~doc:"Allow inclusion between entities of a type."
    "in-allow";
  in_disallow_t, Term.info ~docs:in_scn
    ~doc:"Disallow inclusion between entities of a type."
    "in-disallow";
  in_list_t, Term.info ~docs:in_scn
    ~doc:"Show inclusion policy between types."
    "in-list";

  at_create_t, Term.info ~docs:at_scn
    ~doc:"Create an attribute type."
    "at-create";
  at_delete_t, Term.info ~docs:at_scn
    ~doc:"Delete an attribute type."
    "at-delete";
  at_list_t, Term.info ~docs:at_scn
    ~doc:"List all attribute types."
    "at-list";

  au_force_t, Term.info ~docs:au_scn
    ~doc:"Add an attribute uniqueness constraint."
    "au-force";
  au_relax_t, Term.info ~docs:au_scn
    ~doc:"Remove an attribute uniqueness constraint."
    "au-relax";
  au_list_t, Term.info ~docs:au_scn
    ~doc:"List all attribute uniqueness constraints."
    "au-list";

  an_allow_t, Term.info ~docs:an_scn
    ~doc:"Allow an attribution."
    "an-allow";
  an_disallow_t, Term.info ~docs:an_scn
    ~doc:"Disallow an attribution."
    "an-disallow";
  an_list_t, Term.info ~docs:an_scn
    ~doc:"List allowed attribution."
    "an-list";

  e_ls_t, Term.info ~docs:e_scn
    ~doc:"List entities reachable from a path."
    "ls";
  e_search_t, Term.info ~docs:e_scn
    ~doc:"List entities matching a selector."
    "search";
  e_fts_t, Term.info ~docs:e_scn
    ~doc:"Full-text search."
    "fts";
  e_create_t, Term.info ~docs:e_scn
    ~doc:"Create an entity."
    "create";
  e_delete_t, Term.info ~docs:e_scn
    ~doc:"Delete an entity."
    "delete";
  e_modify_t, Term.info ~docs:e_scn
    ~doc:"Modify an entity."
    "modify";

  load_t, Term.info
    ~doc:"Add, modify, and delete attributes according to the schema loaded \
          from PATH."
    "load"
]

let main_cmd =
  Term.(ret @@ pure (`Error (true, "Missing subcommand."))),
  Term.info ~version:Subsocia_version.pkg_version "subsocia"

let () =
  Dynlink.allow_unsafe_modules true;
  match Term.eval_choice main_cmd subcommands with
  | `Error `Parse -> exit 64
  | `Error `Term -> exit 69
  | `Error `Exn -> exit 70
  | `Ok rc -> exit rc
  | `Version | `Help -> exit 0
