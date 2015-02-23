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

open Cmdliner
open Printf
open Subsocia_common
open Unprime

let connect () =
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  Subsocia_direct.connect uri

let run f = Lwt_main.run (f (connect ()))

let et_create etn = run @@ fun (module C) ->
  lwt et = C.Entity_type.create etn in
  Lwt_log.info_f "Created type #%ld = %s." (C.Entity_type.id et) etn

let et_create_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None &
	 info [] ~docv:"TYPE-NAME" ~doc:"Name of type to create.") in
  Term.(pure et_create $ et_name_t)

let et_delete etn = run @@ fun (module C) ->
  match_lwt C.Entity_type.of_name etn with
  | None ->
    Lwt.return (`Error (false, sprintf "No type is named %s." etn))
  | Some et ->
    let et_id = C.Entity_type.id et in
    C.Entity_type.delete et >>
    Lwt_log.info_f "Deleted type #%ld = %s." et_id etn >>
    Lwt.return (`Ok ())

let et_delete_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None & info ~docv:"TYPE-NAME" []) in
  Term.(ret (pure et_delete $ et_name_t))

let et_list () = run @@ fun (module C) ->
  C.Entity_type.all () >>=
  C.Entity_type.Set.iter_s
    (fun et -> C.Entity_type.name et >>= Lwt_io.printf "%s\n")

let et_list_t = Term.(pure et_list $ pure ())

let subcommands = [
  et_list_t, Term.info ~doc:"List entity types." "et-list";
  et_create_t, Term.info ~doc:"Create an entity type." "et-create";
  et_delete_t, Term.info ~doc:"Delete an entity type." "et-delete";
]

let main_t = Term.(ret @@ pure (`Error (true, "Missing subcommand.")))

let () =
  match Term.eval_choice (main_t, Term.info "subsocia") subcommands with
  | `Error `Parse -> exit 64
  | `Error `Term -> exit 69
  | `Error `Exn -> exit 70
  | `Ok () | `Version | `Help -> exit 0
