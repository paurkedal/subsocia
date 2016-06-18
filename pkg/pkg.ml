#! /usr/bin/env ocaml
#use "topfind"
#require "adpkg"
#require "topkg"
#require "unix"

open Adpkg
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

let () = Pkg.describe "subsocia" ~licenses ~opams ~build ~distrib @@ fun c ->
  Modules.of_file "lib/subsocia.oclib"
    >>= fun subsocia_modules ->
  Modules.of_file "lib/data/subsocia-data.oclib"
    >>= fun subsocia_data_modules ->
  Modules.of_file "web/server/subsocia-web-server.oclib"
    >>= fun web_server_modules ->
  Modules.of_file "web/server/subsocia-web-module.oclib"
    >>= fun web_module_modules ->
  Modules.of_file "web/server/subsocia-web-debug-module.oclib"
    >>= fun web_debug_module_modules ->
  Modules.mllib subsocia_modules "lib/subsocia.mllib"
    >>= fun subsocia_mllib ->
  Modules.mllib subsocia_data_modules
    ~dst_dir:"data/" "lib/data/subsocia-data.mllib"
    >>= fun subsocia_data_mllib ->
  Modules.mllib web_server_modules
    ~dst_dir:"web/server/" "web/server/subsocia-web-server.mllib"
    >>= fun web_server_mllib ->
  Modules.mllib web_module_modules
    ~dst_dir:"web/server/" "web/server/subsocia-web-module.mllib"
    >>= fun web_module_mllib ->
  Modules.mllib web_debug_module_modules
    ~dst_dir:"web/server/" "web/server/subsocia-web-debug-module.mllib"
    >>= fun web_debug_module_mllib ->
  let mllibs = [
    subsocia_mllib;
    subsocia_data_mllib;
    web_server_mllib;
    web_module_mllib;
    web_debug_module_mllib;
  ] in
  let doc_modules =
    let (++) = Modules.union in
    subsocia_modules ++ subsocia_data_modules
      ++ web_server_modules
      ++ web_module_modules ++ web_debug_module_modules in
  Modules.save doc_modules "doc/api.odocl"
               ~filter:Filter.(not (tagged "internal")) >>= fun () ->
  Modules.save doc_modules "doc/dev.odocl" >>= fun () ->
  let misc_fields = [
    Pkg.share ~dst:"static/" "web/client/sociaweb_app.js";
    Pkg.share ~dst:"static/css/" "web/static/css/subsocia.css";
    Pkg.share ~dst:"schema/" "schema/subsocia_tables.sql";
    Pkg.share ~dst:"schema/" "schema/subsocia_views.sql";
    Pkg.share ~dst:"schema/" "schema/subsocia_core.sscm";
    Pkg.bin ~dst:"subsocia" "bin/command";
  ] in
  let upgrade_sources =
    match OS.Dir.contents "schema/upgrade" with
    | Error (`Msg msg) -> failwith msg
    | Ok files -> List.filter (Fpath.has_ext ".sql") files in
  let upgrade_fields =
    List.map (Pkg.share ~dst:"schema/upgrade/") upgrade_sources in
  Ok (mllibs @ misc_fields @ upgrade_fields)
