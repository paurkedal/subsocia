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
open Lwt.Syntax
open Subsocia_cmdliner
open Subsocia_common

let docs = "ATTRIBUTE TYPE COMMANDS"

let at_create (Type.Any vt) atn mult = run_exn @@ fun (module C) ->
  C.Attribute_type.create ~mult vt atn >>= fun at ->
  let* at_idstr = C.Attribute_type.(soid at >|= Soid.to_string) in
  Lwt_log.info_f "Created attribute type %s %s." at_idstr atn

let at_create_cmd =
  let atn =
    let doc = "A name to identify the new attribute type." in
    Arg.(required & pos 0 (some string) None & info ~docv:"NAME" ~doc [])
  in
  let vt =
    let doc = "The type of values of this attribute." in
    Arg.(required & pos 1 (some value_type) None & info ~docv:"TYPE" ~doc [])
  in
  let mu =
    let docv = "MULTIPLICITY" in
    let doc = "The multiplicity of values accepted for this attribute." in
    Arg.(value & pos 2 multiplicity Multiplicity.May & info ~docv ~doc [])
  in
  let term = Term.(const at_create $ vt $ atn $ mu) in
  let info = Cmd.info ~docs ~doc:"Create an attribute type." "at-create" in
  Cmd.v info term

let at_delete atn = run_int_exn @@ fun (module C) ->
  (match%lwt C.Attribute_type.any_of_name_exn atn with
   | exception Subsocia_error.Exn (`Attribute_type_missing _) ->
      Lwt_log.error_f "No attribute type is named %s." atn >>= fun () ->
      Lwt.return 1
   | C.Attribute_type.Any at ->
      C.Attribute_type.delete at >>= fun () ->
      let* at_idstr = C.Attribute_type.(soid at >|= Soid.to_string) in
      Lwt_log.info_f "Delete attribute type %s %s." at_idstr atn >>= fun () ->
      Lwt.return 0)

let at_delete_cmd =
  let atn =
    let doc = "Name of the attribute to delete." in
    Arg.(required & pos 0 (some string) None & info ~docv:"NAME" ~doc [])
  in
  let term = Term.(const at_delete $ atn) in
  let info = Cmd.info ~docs ~doc:"Delete an attribute type." "at-delete" in
  Cmd.v info term

let at_list verbose = run_exn @@ fun (module C) ->
  let show (C.Attribute_type.Any at) =
    let* atn = C.Attribute_type.name at in
    let ms =
      match C.Attribute_type.value_mult at with
      | Multiplicity.Must1 -> ""
      | m -> Multiplicity.to_string m in
    let vt = C.Attribute_type.value_type at in
    Lwt_io.printlf "%s : %s%s" atn (Type.to_string vt) ms >>= fun () ->
    if%lwt Lwt.return verbose then begin
      let show_mapping (et0, et1) =
        let* etn0 = C.Entity_type.name et0 in
        let* etn1 = C.Entity_type.name et1 in
        Lwt_io.printlf "  %s -> %s" etn0 etn1 in
      C.Entity_type.allowed_mappings at >>= Lwt_list.iter_s show_mapping
    end in
  C.Attribute_type.all () >>= C.Attribute_type.Set.iter_s show

let at_list_cmd =
  let verbose =
    let doc = "Show allowed domain and codomain combinations." in
    Arg.(value & flag & info ~doc ["v"])
  in
  let term = Term.(const at_list $ verbose) in
  let info = Cmd.info ~docs ~doc:"List all attribute types." "at-list" in
  Cmd.v info term
