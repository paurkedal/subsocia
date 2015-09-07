(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

let et_create etn = run0 @@ fun (module C) ->
  lwt et = C.Entity_type.create etn in
  Lwt_log.info_f "Created type #%ld = %s." (C.Entity_type.id et) etn

let et_create_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None &
	 info [] ~docv:"ET-NAME" ~doc:"Name of type to create.") in
  Term.(pure et_create $ et_name_t)

let et_modify etn ent_opt = run @@ fun (module C) ->
  match_lwt C.Entity_type.of_name etn with
  | None ->
    Lwt.return (`Error (false, sprintf "No type is named %s." etn))
  | Some et ->
    begin match ent_opt with
    | None -> Lwt.return_unit
    | Some ent -> C.Entity_type.set_entity_name_tmpl et ent
    end >>
    Lwt.return (`Ok 0)

let et_modify_t =
  let etn_t =
    Arg.(required & pos 0 (some string) None &
	 info ~docv:"ET-NAME" ~doc:"Name of the entity type to modify" []) in
  let ent_t =
    Arg.(value & opt (some string) None &
	 info ~docv:"TEMPLATE"
	      ~doc:"Template for the display name of entities of this type."
	      ["name-template"]) in
  Term.(ret (pure et_modify $ etn_t $ ent_t))

let et_delete etn = run @@ fun (module C) ->
  match_lwt C.Entity_type.of_name etn with
  | None ->
    Lwt.return (`Error (false, sprintf "No type is named %s." etn))
  | Some et ->
    let et_id = C.Entity_type.id et in
    C.Entity_type.delete et >>
    Lwt_log.info_f "Deleted type #%ld = %s." et_id etn >>
    Lwt.return (`Ok 0)

let et_delete_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None &
	 info ~docv:"ET-NAME" ~doc:"Name of the entity to delete" []) in
  Term.(ret (pure et_delete $ et_name_t))

let et_list () = run0 @@ fun (module C) ->
  C.Entity_type.all () >>=
  C.Entity_type.Set.iter_s
    (fun et -> C.Entity_type.name et >>= Lwt_io.printl)

let et_list_t = Term.(pure et_list $ pure ())

