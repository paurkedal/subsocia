(* Copyright (C) 2014--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf
open Unprime_option
open Unprime_list

let invalid_arg_f fmt = ksprintf invalid_arg fmt

module Lwt_option = struct
  open Lwt.Infix

  let map_s f = function
   | None -> Lwt.return_none
   | Some x -> f x >|= Option.some

  let iter_s f = function
   | None -> Lwt.return_unit
   | Some x -> f x

end

module Lwt_list = struct
  open Lwt.Infix
  include Lwt_list

  let rec fold_s f = function
   | [] -> Lwt.return
   | x :: xs -> fun acc -> f x acc >>= fold_s f xs

  let rec search_s f = function
   | [] -> Lwt.return None
   | x :: xs ->
      f x >>= (function Some _ as r -> Lwt.return r | None -> search_s f xs)

  let flatten_map_p f xs =
    Lwt_list.rev_map_p f xs >|= fun yss -> List.fold List.rev_append yss []
end
