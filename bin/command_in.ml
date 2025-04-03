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
open Subsocia_common

let docs = "INCLUSION COMMANDS"

let in_allow etn0 etn1 =
  run @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let* et0 = C.Entity_type.of_name etn0 in
  let* et1 = C.Entity_type.of_name etn1 in
  let report_missing etns =
    Lwt.return (`Error (false, "Missing types " ^ etns ^ "."))
  in
  (match et0, et1 with
   | Some et0, Some et1 ->
      let mu0, mu1 = Multiplicity.(May, May) in (* TODO *)
      C.Entity_type.allow_dsub mu0 mu1 et0 et1 >>= fun () ->
      Lwt.return (`Ok 0)
   | None, Some _ -> report_missing etn0
   | Some _, None -> report_missing etn1
   | None, None -> report_missing (etn0 ^ " and " ^ etn1))

let in_disallow etn0 etn1 =
  run @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let* et0 = C.Entity_type.of_name etn0 in
  let* et1 = C.Entity_type.of_name etn1 in
  let report_missing etns =
    Lwt.return (`Error (false, "Missing types " ^ etns ^ "."))
  in
  (match et0, et1 with
   | Some et0, Some et1 ->
      C.Entity_type.disallow_dsub et0 et1 >>= fun () -> Lwt.return (`Ok 0)
   | None, Some _ -> report_missing etn0
   | Some _, None -> report_missing etn1
   | None, None -> report_missing (etn0 ^ " and " ^ etn1))

let in_allow_cmd =
  let etn0 =
    Arg.(required & pos 0 (some string) None & info ~docv:"SUB-TYPE" [])
  in
  let etn1 =
    Arg.(required & pos 1 (some string) None & info ~docv:"SUPER-TYPE" [])
  in
  let term = Term.(ret (const in_allow $ etn0 $ etn1)) in
  let info =
    let doc = "Allow inclusion between entities of a type." in
    Cmd.info ~docs ~doc "in-allow"
  in
  Cmd.v info (with_log term)

let in_disallow_cmd =
  let etn0 =
    Arg.(required & pos 0 (some string) None & info ~docv:"SUB-TYPE" [])
  in
  let etn1 =
    Arg.(required & pos 1 (some string) None & info ~docv:"SUPER-TYPE" [])
  in
  let term = Term.(ret (const in_disallow $ etn0 $ etn1)) in
  let info =
    let doc = "Disallow inclusion between entities of a type." in
    Cmd.info ~docs ~doc "in-disallow"
  in
  Cmd.v info (with_log term)

let in_list etn0_opt etn1_opt =
  run_int_exn @@ fun (module C) ->
  let get_et = function
   | None | Some "_" -> Lwt.return_none
   | Some etn ->
      (C.Entity_type.of_name etn >>= function
       | None -> Lwt.fail (Failure ("No entity type is named " ^ etn ^ "."))
       | Some et -> Lwt.return (Some et))
  in
  let* et0 = get_et etn0_opt in
  let* et1 = get_et etn1_opt in
  let pp mu0 mu1 et0 et1 =
    let* etn0 = C.Entity_type.name et0 in
    let* etn1 = C.Entity_type.name et1 in
    Lwt_io.printlf "%30s %s%s %s"
      etn0 (Multiplicity.to_string mu0) (Multiplicity.to_string mu1) etn1
  in
  (match et0, et1 with
   | None, None ->
      C.Entity_type.dsub_elements () >>=
        Lwt_list.iter_s (fun (et0, et1, mu0, mu1) -> pp mu0 mu1 et0 et1)
        >>= fun () ->
      Lwt.return 0
   | Some et0, None ->
      C.Entity_type.dsuper et0 >>=
        C.Entity_type.Map.iter_s (fun et1 (mu0, mu1) -> pp mu0 mu1 et0 et1)
        >>= fun () ->
      Lwt.return 0
   | None, Some et1 ->
      C.Entity_type.dsub et1 >>=
        C.Entity_type.Map.iter_s (fun et0 (mu0, mu1) -> pp mu0 mu1 et0 et1)
        >>= fun () ->
      Lwt.return 0
   | Some et0, Some et1 ->
      (C.Entity_type.can_dsub et0 et1 >>= function
       | Some (mu0, mu1) ->
          Lwt_io.printlf "%s%s"
            (Multiplicity.to_string mu0)
            (Multiplicity.to_string mu1) >>= fun () ->
          Lwt.return 0
       | None ->
          Lwt.return 1))

let in_list_cmd =
  let etn0 =
    Arg.(value & pos 0 (some string) None & info ~docv:"SUB-TYPE" [])
  in
  let etn1 =
    Arg.(value & pos 1 (some string) None & info ~docv:"SUPER-TYPE" [])
  in
  let term = Term.(const in_list $ etn0 $ etn1) in
  let info =
    let doc = "Show inclusion policy between types." in
    Cmd.info ~docs ~doc "in-list"
  in
  Cmd.v info (with_log term)
