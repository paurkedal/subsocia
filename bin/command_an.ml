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
open Subsocia_cmdliner
open Subsocia_common

let req what name = function
  | None -> Lwt.fail (Failure ("There is no " ^ what ^ " named " ^ name ^ "."))
  | Some x -> Lwt.return x

let an_allow atn etn0 etn1 mu = run0 @@ fun (module C) ->
  lwt at = C.Attribute_type.of_name atn >>= req "attribute type" atn in
  lwt et0 = C.Entity_type.of_name etn0 >>= req "entity type" etn0 in
  lwt et1 = C.Entity_type.of_name etn1 >>= req "entity type" etn1 in
  C.Entity_type.allow_asub et0 et1 at mu

let an_allow_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"NAME" []) in
  let etn0_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 2 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  let mu = Arg.(value & pos 3 multiplicity_conv Multiplicity.May1 &
		info ~docv:"MULTIPLICITY" []) in
  Term.(pure an_allow $ atn_t $ etn0_t $ etn1_t $ mu)

let an_disallow atn etn0 etn1 = run0 @@ fun (module C) ->
  lwt at = C.Attribute_type.of_name atn >>= req "attribute type" atn in
  lwt et0 = C.Entity_type.of_name etn0 >>= req "entity type" etn0 in
  lwt et1 = C.Entity_type.of_name etn1 >>= req "entity type" etn1 in
  C.Entity_type.disallow_asub et0 et1 at

let an_disallow_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"NAME" []) in
  let etn0_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 2 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(pure an_disallow $ atn_t $ etn0_t $ etn1_t)

let an_list () = run0 @@ fun (module C) ->
  C.Entity_type.asub_elements () >>=
  Lwt_list.map_s @@ fun (et0, et1, C.Attribute_type.Ex at, mu) ->
  lwt atn = C.Attribute_type.name' at in
  lwt etn0 = C.Entity_type.name et0 in
  lwt etn1 = C.Entity_type.name et1 in
  Lwt_io.printlf "%s %s %s %s" (Multiplicity.to_string mu) atn etn0 etn1

let an_list_t = Term.(pure an_list $ pure ())
