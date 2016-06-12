(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

#require "topkg"
open Printf
open Topkg

exception Parse_error of string * int * string

let parse_error fp lnum msg = raise (Parse_error (fp, lnum, msg))

let save_lines fn lns =
  let oc = open_out fn in List.iter (fprintf oc "%s\n") lns; close_out oc

module Ad_os_file = struct
  let with_open_in f fp =
    let ic = open_in fp in
    try
      let y = f ic in
      close_in ic; y
    with exc ->
      close_in ic; raise exc

  let fold_lines f fp acc =
    fp |> with_open_in @@ fun ic ->
      let rec loop acc =
        try loop (f (input_line ic) acc) with End_of_file -> acc in
      loop acc
end

module Library = struct

  type mspec = string * string list

  type t = {
    dst_dir : string option;
    libpath : string;
    mspecs : mspec list;
  }

  let load_mspecs fp =
    let aux line (lnum, acc) =
      let line =
        match String.cut ~sep:'#' line with
        | None -> String.trim line
        | Some (s, _) -> String.trim s in
      if line = "" then (succ lnum, acc) else
      let m, flags =
        match String.cut ~sep:' ' line with
        | None -> line, []
        | Some (m, s) ->
          let s = String.trim s in
          let n = String.length s in
          if n = 0 then m, [] else
          if s.[0] <> '{' || s.[n - 1] <> '}' then
            parse_error fp lnum "Incomprehensible litter after module name."
          else
            let s = String.with_index_range ~first:1 ~last:(n - 2) s in
            m, List.map String.trim (String.cuts ~sep:',' s) in
      (succ lnum, (m, flags) :: acc) in
    Ad_os_file.fold_lines aux fp (1, [])

  let create ?dst_dir libpath =
    let _, mspecs = load_mspecs (libpath ^ ".olib") in
    {libpath; dst_dir; mspecs}

  let modules ?filter ?(qualify = false) lib =
    let mspecs =
      match filter with
      | None -> lib.mspecs
      | Some f -> List.filter (fun (_, flags) -> f flags) lib.mspecs in
    let dir =
      if not qualify then None else
      let dir = Fpath.dirname lib.libpath in
      if dir = "." then None else Some dir in
    match dir with
    | Some dir -> List.map (fun (m, _) -> Filename.concat dir m) mspecs
    | None -> List.map fst mspecs

  let all_modules ?qualify lib = modules ?qualify lib

  let api_modules ?qualify lib =
    let not_excl = function "internal" -> false | _ -> true in
    modules ?qualify ~filter:(List.for_all not_excl) lib

  let mllib lib =
    let mllibpath = lib.libpath ^ ".mllib" in
    save_lines mllibpath (modules lib);
    Pkg.mllib ~api:(api_modules lib) ?dst_dir:lib.dst_dir mllibpath
end
