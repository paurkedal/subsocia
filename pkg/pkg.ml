#! /usr/bin/env ocaml
#use "topfind"
#require "adpkg"
#require "topkg"
#require "unix"

open Adpkg
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let opams = [
  Pkg.opam_file
    ~lint_deps_excluding:(Some ["adpkg"; "ounit"; "oUnit"; "rpc"; "rpclib"])
    "subsocia.opam"
]

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(eliom.ocamlbuild)"
        % "-build-dir" % build_dir
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let () = Pkg.describe "subsocia" ~licenses ~opams ~build @@ fun c ->
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
    Pkg.test "tests/testsuite";
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
