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

let docs = "ENTITY TYPE COMMANDS"

let et_name_t =
  let doc = "The name of the entity type." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"ET-NAME" ~doc)

let et_info etn =
  run @@ fun (module C) ->
  (C.Entity_type.of_name etn >>= function
   | None ->
      Lwt.return (`Error (false, sprintf "No entity type is named %s." etn))
   | Some et ->
      let* soid = C.Entity_type.soid et in
      let* name_tmpl = C.Entity_type.entity_name_tmpl et in
      Lwt_io.printlf "Entity type #%ld %s " soid etn >>= fun () ->
      Lwt_io.printlf "Name template: %s" name_tmpl >>= fun () ->
      Lwt.return (`Ok 0))

let et_info_cmd =
  let term = Term.(ret (const et_info $ et_name_t)) in
  let info =
    let doc = "Show information about the named entity type." in
    Cmd.info ~docs ~doc "et-info"
  in
  Cmd.v info (with_log term)

let et_create etn =
  run_exn @@ fun (module C) ->
  let* et = C.Entity_type.create etn in
  let* et_idstr = C.Entity_type.(soid et >|= Soid.to_string) in
  Log.info (fun f -> f "Created type %s = %s." et_idstr etn)

let et_create_cmd =
  let term = Term.(const et_create $ et_name_t) in
  let info = Cmd.info ~docs ~doc:"Create an entity type." "et-create" in
  Cmd.v info (with_log term)

let et_modify etn ent_opt =
  run @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  (C.Entity_type.of_name etn >>= function
   | None ->
      Lwt.return (`Error (false, sprintf "No entity type is named %s." etn))
   | Some et ->
      (match ent_opt with
       | None -> Lwt.return_unit
       | Some ent -> C.Entity_type.set_entity_name_tmpl et ent) >>= fun () ->
      Lwt.return (`Ok 0))

let et_modify_cmd =
  let etn =
    let doc = "Name of the entity type to modify" in
    Arg.(required & pos 0 (some string) None & info ~docv:"ET-NAME" ~doc [])
  in
  let display =
    let docv = "TEMPLATE" in
    let doc = "Template for the display name of entities of this type." in
    Arg.(value & opt (some string) None & info ~docv ~doc ["name-template"])
  in
  let term = Term.(ret (const et_modify $ etn $ display)) in
  let info = Cmd.info ~docs ~doc:"Modify an entity type." "et-modify" in
  Cmd.v info (with_log term)

let et_delete etn =
  run @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  (C.Entity_type.of_name etn >>= function
   | None ->
      Lwt.return (`Error (false, sprintf "No type is named %s." etn))
   | Some et ->
      let* et_id = C.Entity_type.soid et in
      C.Entity_type.delete et >>= fun () ->
      Log.info (fun f -> f "Deleted type #%ld = %s." et_id etn) >>= fun () ->
      Lwt.return (`Ok 0))

let et_delete_cmd =
  let et_name_t =
    let doc = "Name of the entity to delete" in
    Arg.(required & pos 0 (some string) None & info ~docv:"ET-NAME" ~doc [])
  in
  let term = Term.(ret (const et_delete $ et_name_t)) in
  let info = Cmd.info ~docs ~doc:"Delete an entity type." "et-delete" in
  Cmd.v info (with_log term)

let et_list () =
  run_exn @@ fun (module C) ->
  C.Entity_type.all () >>=
  C.Entity_type.Set.iter_s (fun et -> C.Entity_type.name et >>= Lwt_io.printl)

let et_list_cmd =
  let term = Term.(const et_list $ const ()) in
  let info = Cmd.info ~docs ~doc:"List entity types." "et-list" in
  Cmd.v info (with_log term)
