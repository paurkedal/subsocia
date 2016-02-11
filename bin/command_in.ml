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
open Subsocia_common

let in_allow etn0 etn1 = run @@ fun (module C) ->
  let%lwt et0 = C.Entity_type.of_name etn0 in
  let%lwt et1 = C.Entity_type.of_name etn1 in
  let report_missing etns =
    Lwt.return (`Error (false, "Missing types " ^ etns ^ ".")) in
  match et0, et1 with
  | Some et0, Some et1 ->
    let mu0, mu1 = Multiplicity.(May, May) in (* TODO *)
    C.Entity_type.allow_dsub mu0 mu1 et0 et1 >>
    Lwt.return (`Ok 0)
  | None, Some _ -> report_missing etn0
  | Some _, None -> report_missing etn1
  | None, None -> report_missing (etn0 ^ " and " ^ etn1)

let in_disallow etn0 etn1 = run @@ fun (module C) ->
  let%lwt et0 = C.Entity_type.of_name etn0 in
  let%lwt et1 = C.Entity_type.of_name etn1 in
  let report_missing etns =
    Lwt.return (`Error (false, "Missing types " ^ etns ^ ".")) in
  match et0, et1 with
  | Some et0, Some et1 -> C.Entity_type.disallow_dsub et0 et1 >>
                          Lwt.return (`Ok 0)
  | None, Some _ -> report_missing etn0
  | Some _, None -> report_missing etn1
  | None, None -> report_missing (etn0 ^ " and " ^ etn1)

let in_allow_t =
  let etn0_t = Arg.(required & pos 0 (some string) None &
                    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 1 (some string) None &
                    info ~docv:"SUPER-TYPE" []) in
  Term.(ret (pure in_allow $ etn0_t $ etn1_t))

let in_disallow_t =
  let etn0_t = Arg.(required & pos 0 (some string) None &
                    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 1 (some string) None &
                    info ~docv:"SUPER-TYPE" []) in
  Term.(ret (pure in_disallow $ etn0_t $ etn1_t))

let in_list etn0_opt etn1_opt = run @@ fun (module C) ->
  let get_et = function
    | None | Some "_" -> Lwt.return_none
    | Some etn ->
      begin match%lwt C.Entity_type.of_name etn with
      | None -> Lwt.fail (Failure ("No entity type is named " ^ etn ^ "."))
      | Some et -> Lwt.return (Some et)
      end in
  let%lwt et0 = get_et etn0_opt in
  let%lwt et1 = get_et etn1_opt in
  let pp mu0 mu1 et0 et1 =
    let%lwt etn0 = C.Entity_type.name et0 in
    let%lwt etn1 = C.Entity_type.name et1 in
    Lwt_io.printlf "%30s %s%s %-30s"
      etn0 (Multiplicity.to_string mu0) (Multiplicity.to_string mu1) etn1 in
  match et0, et1 with
  | None, None ->
    C.Entity_type.dsub_elements () >>=
      Lwt_list.iter_s (fun (et0, et1, mu0, mu1) -> pp mu0 mu1 et0 et1) >>
    Lwt.return 0
  | Some et0, None ->
    C.Entity_type.dsuper et0 >>=
      C.Entity_type.Map.iter_s (fun et1 (mu0, mu1) -> pp mu0 mu1 et0 et1) >>
    Lwt.return 0
  | None, Some et1 ->
    C.Entity_type.dsub et1 >>=
      C.Entity_type.Map.iter_s (fun et0 (mu0, mu1) -> pp mu0 mu1 et0 et1) >>
    Lwt.return 0
  | Some et0, Some et1 ->
    begin match%lwt C.Entity_type.can_dsub et0 et1 with
    | Some (mu0, mu1) ->
      Lwt_io.printlf "%s%s" (Multiplicity.to_string mu0)
                            (Multiplicity.to_string mu1) >>
      Lwt.return 0
    | None ->
      Lwt.return 1
    end

let in_list_t =
  let etn0_t = Arg.(value & pos 0 (some string) None &
                    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(value & pos 1 (some string) None &
                    info ~docv:"SUPER-TYPE" []) in
  Term.(pure in_list $ etn0_t $ etn1_t)

