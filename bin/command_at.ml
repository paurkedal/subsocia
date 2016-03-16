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
open Lwt.Infix
open Subsocia_cmdliner
open Subsocia_common

let at_create (Type.Ex vt) atn mult = run0 @@ fun (module C) ->
  C.Attribute_type.create ~mult vt atn >>= fun at ->
  Lwt_log.info_f "Created attribute type #%ld %s." (C.Attribute_type.id at) atn

let at_create_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
                   info ~docv:"NAME" []) in
  let vt_t = Arg.(required & pos 1 (some value_type_conv) None &
                  info ~docv:"TYPE" []) in
  let mu_t = Arg.(value & pos 2 multiplicity_conv Multiplicity.May1 &
                  info ~docv:"MULTIPLICITY" []) in
  Term.(pure at_create $ vt_t $ atn_t $ mu_t)

let at_delete atn = run @@ fun (module C) ->
  match%lwt C.Attribute_type.of_name atn with
  | Some (C.Attribute_type.Ex at) ->
    C.Attribute_type.delete at >>
    Lwt_log.info_f "Delete attribute type #%ld %s."
                   (C.Attribute_type.id at) atn >>
    Lwt.return 0
  | None ->
    Lwt_log.error_f "No attribute type is named %s." atn >>
    Lwt.return 1

let at_delete_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
                   info ~docv:"NAME" []) in
  Term.(pure at_delete $ atn_t)

let at_list verbose = run @@ fun (module C) ->
  let show (C.Attribute_type.Ex at) =
    let%lwt atn = C.Attribute_type.name at in
    let ms =
      match C.Attribute_type.value_mult at with
      | Multiplicity.Must1 -> ""
      | m -> Multiplicity.to_string m in
    let vt = C.Attribute_type.value_type at in
    Lwt_io.printlf "%s : %s%s" atn (Type.to_string vt) ms >>
    if%lwt Lwt.return verbose then begin
      let show_mapping (et0, et1) =
        let%lwt etn0 = C.Entity_type.name et0 in
        let%lwt etn1 = C.Entity_type.name et1 in
        Lwt_io.printlf "  %s -> %s" etn0 etn1 in
      C.Entity_type.allowed_mappings at >>= Lwt_list.iter_s show_mapping
    end in
  C.Attribute_type.all () >>= C.Attribute_type.Set.iter_s show >>
  Lwt.return 0

let at_list_t =
  let doc = "Show allowed domain and codomain combinations." in
  let v = Arg.(value & flag & info ~doc ["v"]) in
  Term.(pure at_list $ v)
