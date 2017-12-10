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
open Lwt.Infix
open Panograph_i18n
open Subsocia_common

let connect () =
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  Subsocia_direct.connect uri

let langs = [Lang.of_string "en"] (* TODO: Use $LANG *)

let run f = Lwt_main.run (f (connect ()))
let run0 f = Lwt_main.run (f (connect ())); 0

let run_int_exn f = Lwt_main.run
  (try%lwt f (connect ()) with
   | Caqti_error.Exn err ->
      Lwt_io.printl (Caqti_error.show err) >|= fun () -> 69)

let run_exn f = run_int_exn (fun c -> f c >|= fun () -> 0)

let run_bool_exn f =
  run_int_exn (fun c -> f c >|= function false -> 0 | true -> 1)

let disable_transaction_t =
  Arg.(value & flag &
       info ~doc:"Commit changes one at a time instead of as a single \
                  transaction." ["disable-transaction"])
