(* Copyright (C) 2022--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt.Infix

module Verbosity = struct
  type t = {
    global: Logs.level option option;
    per_source: (string * Logs.level option) list;
  }

  let default = {global = None; per_source = []}

  let merge v1 v2 =
    let global =
      (match v1.global, v2.global with
       | None, None -> None
       | Some global, _ | None, Some global -> Some global)
    in
    let per_source = v1.per_source @ v2.per_source in
    {global; per_source}

  let level_of_string_exn level_name =
    (match Logs.level_of_string level_name with
     | Ok level -> level
     | Error (`Msg msg) -> failwith msg)

  let of_string s =
    try
      let parse_pair x =
        let i = String.rindex x ':' in
        let level_name = String.sub x (i + 1) (String.length x - i - 1) in
        (String.sub x 0 i, level_of_string_exn level_name)
      in
      let parse_per_source src_levels =
        let compare (pfx1, _) (pfx2, _) =
          if String.length pfx1 < String.length pfx2 then 1 else
          if String.length pfx1 > String.length pfx2 then -1 else
          String.compare pfx1 pfx2
        in
        src_levels
          |> List.rev_map parse_pair
          |> List.sort_uniq compare
      in
      (match String.split_on_char ',' s with
       | [] ->
          Ok {global = None; per_source = []}
       | "" :: src_levels ->
          Ok {global = None; per_source = parse_per_source src_levels}
       | level :: src_levels when not (String.contains level ':') ->
          let global = level_of_string_exn level in
          Ok {global = Some global; per_source = parse_per_source src_levels}
       | src_levels ->
          Ok {global = None; per_source = parse_per_source src_levels})
    with
     | Failure msg -> Error (`Msg ("invalid verbosity: " ^ msg))
     | Not_found -> Error (`Msg "missing colon in verbosity specification")

  let pp =
    let open Fmt in
    let level = option ~none:(const string "quiet") Logs.pp_level in
    using (fun {global; _} -> global) (option level) ++
    using (fun {per_source; _} -> per_source)
      (list (comma ++ pair ~sep:(const string ":") string level))

  let to_string = Fmt.to_to_string pp

  let cmdliner_conv = Cmdliner.Arg.conv (of_string, pp)

  let cmdliner_term =
    let open Cmdliner in
    let doc =
      {|Set log default and per-source verbosity according to $(docv), which is
        a comma-separated list of an optional LEVEL specifying the default log
        level, followed by zero or more SRC:LEVEL entries where SRC is a source
        prefix and LEVEL is the level to set for matching sources.

        A prefix
        matches any source whose name equals the prefix, optionally followed by
        a period and an arbitrary string.  In case of multiple matches, the
        longest matching prefix determines the source level.|}
    in
    let docv = "LEVEL-SPECS" in
    let env = Cmd.Env.info ~doc "SUBSOCIA_LOG_VERBOSITY" in
    Arg.(value @@ opt cmdliner_conv default @@
         info ~env ~doc ~docv ["verbosity"])

  let to_yojson v = `String (to_string v)

  let of_yojson = function
   | `String s -> of_string s |> Result.map_error (function `Msg msg -> msg)
   | _ -> Error "The log verbosity must be a string."
end

let pp_ptime = Ptime.pp_rfc3339 ~tz_offset_s:0 ()

let style_of_level = function
 | Logs.App -> Logs_fmt.app_style
 | Logs.Error -> Logs_fmt.err_style
 | Logs.Warning -> Logs_fmt.warn_style
 | Logs.Info -> Logs_fmt.info_style
 | Logs.Debug -> Logs_fmt.debug_style

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    let flush () =
      let m = Buffer.contents b in
      Buffer.reset b; m
    in
    (Fmt.with_buffer ~like b, flush)
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let report src level ~over k msgf =
    let k _ppf =
      let write () = match level with
       | Logs.App ->
          Lwt_io.write Lwt_io.stdout (app_flush ()) >>= fun () ->
          Lwt_io.flush Lwt_io.stdout
       | _ ->
          Lwt_io.write Lwt_io.stderr (dst_flush ()) >>= fun () ->
          Lwt_io.flush Lwt_io.stderr
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    msgf begin fun ?header:_ ?tags:_ fmt ->
      let ppf = if level = App then app else dst in
      Format.kfprintf k ppf ("%a %a %s: @[" ^^ fmt ^^ "@]@.")
        pp_ptime (Ptime_clock.now ())
        (Fmt.styled (style_of_level level) Logs.pp_level) level
        (Logs.Src.name src)
    end
  in
  {Logs.report = report}

let setup ?(verbosity = Verbosity.default) () =
  Logs.set_reporter (lwt_reporter ());
  let setup_src src =
    let src_name = Logs.Src.name src in
    let src_covered_by (prefix, _) =
      if src_name = prefix then true else
      let l = String.length prefix in
      l < String.length src_name
        && String.sub src_name 0 l = prefix
        && src_name.[l] = '.'
    in
    Option.iter
      (fun (_, level) -> Logs.Src.set_level src level)
      (List.find_opt src_covered_by verbosity.per_source)
  in
  Option.iter Logs.set_level verbosity.global;
  List.iter setup_src (Logs.Src.list ())
