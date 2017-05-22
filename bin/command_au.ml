(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Subsocia_prereq
open Unprime_list

module type S = sig
  include Subsocia_derived_intf.S
  val attribute_type_of_arg : string -> Attribute_type.ex Lwt.t
end

module Make (Base : Subsocia_intf.S) : S = struct
  include Subsocia_derived.Make (Base)

  let attribute_type_of_arg atn =
    match%lwt Attribute_type.of_name atn with
    | Some at -> Lwt.return at
    | None -> lwt_failure_f "These is no attribute type named %s." atn
end

let run f =
  Lwt_main.run begin
    let module Base = (val connect ()) in
    let module C = Make (Base) in
    f (module C : S)
  end

let au_force atns = run @@ fun (module C : S) ->
  let%lwt ats = Lwt_list.map_s C.attribute_type_of_arg atns in
  let ats = List.fold C.Attribute_type.Set.add ats C.Attribute_type.Set.empty in
  match%lwt C.Attribute_uniqueness.find ats with
  | None ->
    let%lwt au = C.Attribute_uniqueness.force ats in
    let%lwt au_idstr = C.Attribute_uniqueness.soid_string au in
    Lwt_io.eprintlf "Created constraint %s." au_idstr >>
    Lwt.return 0
  | Some au ->
    let%lwt au_idstr = C.Attribute_uniqueness.soid_string au in
    Lwt_io.eprintlf "Already constrained by %s." au_idstr >>
    Lwt.return 1

let au_force_cmd =
  let atns_t = Arg.(value & pos_all string [] &
                    info ~docv:"ATTRIBUTE-TYPES" []) in
  Term.(pure au_force $ atns_t)

let au_relax atns = run @@ fun (module C : S) ->
  let%lwt ats = Lwt_list.map_s C.attribute_type_of_arg atns in
  let ats = List.fold C.Attribute_type.Set.add ats C.Attribute_type.Set.empty in
  match%lwt C.Attribute_uniqueness.find ats with
  | Some au ->
    C.Attribute_uniqueness.relax au >>
    let%lwt au_idstr = C.Attribute_uniqueness.soid_string au in
    Lwt_io.eprintlf "Removed constraint %s." au_idstr >>
    Lwt.return 0
  | None ->
    Lwt_io.eprintlf "No matching constraint." >>
    Lwt.return 1

let au_relax_cmd =
  let atns_t = Arg.(value & pos_all string [] &
                    info ~docv:"ATTRIBUTE-TYPES" []) in
  Term.(pure au_relax $ atns_t)

let au_list () = run @@ fun (module C : S) ->
  let show_at pos (C.Attribute_type.Ex at) =
    let%lwt () = if !pos = 0 then Lwt.return_unit else Lwt_io.print ", " in
    let () = incr pos in
    let%lwt atn = C.Attribute_type.name at in
    Lwt_io.print atn in
  let show_au au =
    let%lwt ats = C.Attribute_uniqueness.affected au in
    Lwt_io.print "{" >>
    C.Attribute_type.Set.iter_s (show_at (ref 0)) ats >>
    Lwt_io.printl "}" in
  C.Attribute_uniqueness.all () >>= C.Attribute_uniqueness.Set.iter_s show_au >>
  Lwt.return 0

let au_list_cmd = Term.(pure au_list $ pure ())
