(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
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
open Subsocia_common

let docs = "ATTRIBUTION COMMANDS"

let an_allow atn etn0 etn1 = run_exn @@ fun (module C) ->
  let%lwt C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
  let%lwt et0 = C.Entity_type.of_name_exn etn0 in
  let%lwt et1 = C.Entity_type.of_name_exn etn1 in
  C.Entity_type.allow_attribution at et0 et1

let an_allow_cmd =
  let atn =
    Arg.(required & pos 0 (some string) None & info ~docv:"NAME" []) in
  let etn0 =
    Arg.(required & pos 1 (some string) None & info ~docv:"SUB-TYPE" []) in
  let etn1 =
    Arg.(required & pos 2 (some string) None & info ~docv:"SUPER-TYPE" []) in
  let term = Term.(const an_allow $ atn $ etn0 $ etn1) in
  let info = Term.info ~docs ~doc:"Allow an attribution." "an-allow" in
  (term, info)

let an_disallow atn etn0 etn1 = run_exn @@ fun (module C) ->
  let%lwt C.Attribute_type.Any at = C.Attribute_type.any_of_name_exn atn in
  let%lwt et0 = C.Entity_type.of_name_exn etn0 in
  let%lwt et1 = C.Entity_type.of_name_exn etn1 in
  C.Entity_type.disallow_attribution at et0 et1

let an_disallow_cmd =
  let atn =
    Arg.(required & pos 0 (some string) None & info ~docv:"NAME" []) in
  let etn0 =
    Arg.(required & pos 1 (some string) None & info ~docv:"SUB-TYPE" []) in
  let etn1 =
    Arg.(required & pos 2 (some string) None & info ~docv:"SUPER-TYPE" []) in
  let term = Term.(const an_disallow $ atn $ etn0 $ etn1) in
  let info = Term.info ~docs ~doc:"Disallow an attribution." "an-disallow" in
  (term, info)

let an_list () = run_exn @@ fun (module C) ->
  C.Entity_type.allowed_attributions () >>=
  Lwt_list.iter_s @@ fun (C.Attribute_type.Any at, et0, et1) ->
  let mu = C.Attribute_type.value_mult at in
  let%lwt atn = C.Attribute_type.name at in
  let%lwt etn0 = C.Entity_type.name et0 in
  let%lwt etn1 = C.Entity_type.name et1 in
  Lwt_io.printlf "%s %s %s %s" (Multiplicity.to_string mu) atn etn0 etn1

let an_list_cmd =
  let term = Term.(const an_list $ const ()) in
  let info = Term.info ~docs ~doc:"List allowed attribution." "an-list" in
  (term, info)
