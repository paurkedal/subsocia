(* Copyright (C) 2018--2021  Petter A. Urkedal <paurkedal@gmail.com>
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

open Subsocia_common

type t =
  [ `Msg of string
  | `Attribute_type_missing of string
  | `Attribute_type_mismatch of string * Type.any * Type.any
  | `Entity_type_missing of string ]

exception Exn of t

let pp ppf =
  let open Format in
  (function
   | `Msg msg ->
      pp_print_string ppf msg; pp_print_newline ppf ()
   | `Attribute_type_missing atn ->
      fprintf ppf "There is no stored attribute type named %s." atn
   | `Attribute_type_mismatch (atn, (Type.Any vt), (Type.Any vt_req)) ->
      fprintf ppf "Stored attribute type %s has type %a, requested as %a."
        atn Type.pp vt Type.pp vt_req
   | `Entity_type_missing etn ->
      fprintf ppf "There is no stored entity type named %s." etn)

let show error =
  let buf = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer buf in
  pp ppf error;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

let fail_lwt fmt = Format.kasprintf (fun msg -> Lwt.fail (Exn (`Msg msg))) fmt

let () =
  Printexc.register_printer (function Exn err -> Some (show err) | _ -> None)
