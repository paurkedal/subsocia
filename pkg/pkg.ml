#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
#require "unix"
#directory "pkg"
#use "pkg_utils.ml"

open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let opams = [Pkg.opam_file ~lint_deps_excluding:(Some ["lib"]) "opam"]

let build_cmd c os =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-classic-display"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir)
let () = Unix.putenv "OCAMLPATH" "."

let build = Pkg.build ~cmd:build_cmd ()

let prefix =
  Cmd.(v "ocamlfind" % "printconf" % "destdir")
    |> OS.Cmd.run_out
    |> OS.Cmd.to_string
    |> (function Ok s -> s | Error (`Msg msg) -> failwith msg)
    |> Fpath.basename

let pkg_datadir = Fpath.(prefix // "share" // "subsocia")

let watermarks = ("PKG_DATADIR", `String pkg_datadir) :: Pkg.watermarks

let distrib = Pkg.distrib ~watermarks ()

let libs = Library.[
  create "lib/subsocia";
  create "lib/data/subsocia-data" ~dst_dir:"data/";
  create "web/server/subsocia-web-server" ~dst_dir:"web/server/";
  create "web/server/subsocia-web-module" ~dst_dir:"web/server/";
  create "web/server/subsocia-web-debug-module" ~dst_dir:"web/server/";
]
let api_modules =
  List.flatten (List.map (Library.api_modules ~qualify:true) libs)
let all_modules =
  List.flatten (List.map (Library.all_modules ~qualify:true) libs)

let () = save_lines "doc/api.odocl" api_modules
let () = save_lines "doc/dev.odocl" all_modules

let () = Pkg.describe "subsocia" ~licenses ~opams ~build ~distrib @@ fun c ->
  let mllibs = List.map Library.mllib libs in
  let misc_fields = [
    Pkg.share ~dst:"static/" "web/client/sociaweb_app.js";
    Pkg.share ~dst:"static/css/" "web/static/css/subsocia.css";
    Pkg.share ~dst:"schema/" "schema/subsocia_tables.sql";
    Pkg.share ~dst:"schema/" "schema/subsocia_views.sql";
    Pkg.share ~dst:"schema/" "schema/subsocia_core.sscm";
  ] in
  let upgrade_sources =
    match OS.Dir.contents "schema/upgrade" with
    | Error (`Msg msg) -> failwith msg
    | Ok files -> List.filter (Fpath.has_ext ".sql") files in
  let upgrade_fields =
    List.map (Pkg.share ~dst:"schema/upgrade/") upgrade_sources in
  Ok (mllibs @ misc_fields @ upgrade_fields)
