(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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
open Lwt.Syntax

let drop_schema_req schema =
  let open Caqti_request.Infix in
  let open Caqti_type.Std in
  (unit ->. unit) ~oneshot:true
  (Fmt.str "DROP SCHEMA IF EXISTS %s CASCADE" schema)

let () = Lwt_main.run begin
  let uri = Uri.of_string Subsocia_config.(global.database_uri) in
  let schema =
    (match Uri.get_query_param uri "schema" with
     | Some schema -> schema
     | None -> failwith "Won't drop default subsocia schema.")
  in
  let* (module C) =
    Caqti_lwt_unix.connect (Uri.remove_query_param uri "schema")
    >>= Caqti_lwt.or_fail
  in
  C.exec (drop_schema_req schema) () >>= Caqti_lwt.or_fail
end
