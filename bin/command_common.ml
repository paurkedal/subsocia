(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

open Lwt.Infix

let connect () =
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  Subsocia_direct.connect uri

let langs = [Iso639.Lang.of_string_exn "eng"] (* TODO: Use $LANG *)

let run f = Lwt_main.run (f (connect ()))

let run_int_exn f = Lwt_main.run
  (try%lwt f (connect ()) with
   | Failure msg ->
      Lwt_io.printl msg >|= fun () -> 69
   | Caqti_error.Exn err ->
      Lwt_io.printl (Caqti_error.show err) >|= fun () -> 69
   | Subsocia_error.Exn err ->
      Lwt_io.printl (Subsocia_error.show err) >|= fun () -> 69)

let run_exn f = run_int_exn (fun c -> f c >|= fun () -> 0)

let run_bool_exn f =
  run_int_exn (fun c -> f c >|= function false -> 1 | true -> 0)
