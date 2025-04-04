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
open Unprime_list

let docs = "ATTRIBUTE UNIQUENESS"

let au_force atns =
  run_int_exn @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let module C = Subsocia_derived.Make (C) in
  let* ats = Lwt_list.map_s C.Attribute_type.any_of_name_exn atns in
  let ats = List.fold C.Attribute_type.Set.add ats C.Attribute_type.Set.empty in
  (C.Attribute_uniqueness.find ats >>= function
   | None ->
      let* au = C.Attribute_uniqueness.force ats in
      let* au_idstr = C.Attribute_uniqueness.soid_string au in
      Lwt_io.eprintlf "Created constraint %s." au_idstr >>= fun () ->
      Lwt.return 0
   | Some au ->
      let* au_idstr = C.Attribute_uniqueness.soid_string au in
      Lwt_io.eprintlf "Already constrained by %s." au_idstr >>= fun () ->
      Lwt.return 1)

let au_force_cmd =
  let atns =
    Arg.(value & pos_all string [] & info ~docv:"ATTRIBUTE-TYPES" [])
  in
  let term = Term.(const au_force $ atns) in
  let info =
    let doc = "Add an attribute uniqueness constraint." in
    Cmd.info ~docs ~doc "au-force"
  in
  Cmd.v info (with_log term)

let au_relax atns =
  run_int_exn @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let module C = Subsocia_derived.Make (C) in
  let* ats = Lwt_list.map_s C.Attribute_type.any_of_name_exn atns in
  let ats = List.fold C.Attribute_type.Set.add ats C.Attribute_type.Set.empty in
  (C.Attribute_uniqueness.find ats >>= function
   | Some au ->
      C.Attribute_uniqueness.relax au >>= fun () ->
      let* au_idstr = C.Attribute_uniqueness.soid_string au in
      Lwt_io.eprintlf "Removed constraint %s." au_idstr >>= fun () ->
      Lwt.return 0
   | None ->
      Lwt_io.eprintlf "No matching constraint." >>= fun () ->
      Lwt.return 1)

let au_relax_cmd =
  let atns =
    Arg.(value & pos_all string [] & info ~docv:"ATTRIBUTE-TYPES" [])
  in
  let term = Term.(const au_relax $ atns) in
  let info =
    let doc = "Remove an attribute uniqueness constraint." in
    Cmd.info ~docs ~doc "au-relax"
  in
  Cmd.v info (with_log term)

let au_list () = run_exn @@ fun (module C) ->
  let show_at pos (C.Attribute_type.Any at) =
    let* () = if !pos = 0 then Lwt.return_unit else Lwt_io.print ", " in
    let () = incr pos in
    let* atn = C.Attribute_type.name at in
    Lwt_io.print atn
  in
  let show_au au =
    let* ats = C.Attribute_uniqueness.affected au in
    Lwt_io.print "{" >>= fun () ->
    C.Attribute_type.Set.iter_s (show_at (ref 0)) ats >>= fun () ->
    Lwt_io.printl "}"
  in
  C.Attribute_uniqueness.all () >>= C.Attribute_uniqueness.Set.iter_s show_au

let au_list_cmd =
  let term = Term.(const au_list $ const ()) in
  let info =
    let doc = "List all attribute uniqueness constraints." in
    Cmd.info ~docs ~doc "au-list"
  in
  Cmd.v info (with_log term)
