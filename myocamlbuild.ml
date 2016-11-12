open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/sociaweb_app.byte";
]

let sed_rule ~dep ~prod scripts =
  rule (dep ^ " -> " ^ prod) ~dep ~prod
    (fun env build ->
      let dep = env dep and prod = env prod in
      let script_args = List.map (fun script -> S[A"-e"; A script]) scripts in
      Cmd (S[A"sed"; S script_args; P dep; Sh">"; Px prod]))

let ocamlfind_destdir = run_and_read "ocamlfind printconf destdir"
let prefix = Filename.dirname ocamlfind_destdir
let datadir = Filename.concat prefix "share"
let subsocia_datadir = Filename.concat datadir "subsocia"

let pkg_datadir pkgname =
  let libdir = Findlib.((query pkgname).location) in
  let librootdir = Filename.dirname libdir in
  let prefix = Filename.dirname librootdir in
  let datarootdir = Filename.concat prefix "share" in
  Filename.concat datarootdir pkgname

let panograph_datadir = pkg_datadir "panograph"

let local_rules () =
  sed_rule ~dep:"ocsigen-dev.conf.in" ~prod:"ocsigen-dev.conf"
    ["s;@PANOGRAPH_DATADIR@;" ^ pkg_datadir "panograph" ^ ";g"];
  sed_rule ~dep:"lib/subsocia_version.ml.in" ~prod:"lib/subsocia_version.ml"
    ["s;@SUBSOCIA_DATADIR@;" ^ subsocia_datadir ^ ";g"];

  rule "%.mli & %.idem -> %.ml"
    ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
    begin fun env build ->
      let src = env "%.mli" and dst = env "%.ml" in
      cp src dst
    end;
  rule "%.eliomi & %.idem -> %.eliom"
    ~deps:["%.eliomi"; "%.idem"] ~prod:"%.eliom"
    begin fun env build ->
      let src = env "%.eliomi" and dst = env "%.eliom" in
      cp src dst
    end;
  Pathname.define_context "web/type" ["lib/data"];
  Pathname.define_context "web/server" ["lib/data"];
  Pathname.define_context "bin" ["lib/data"];
  Pathname.define_context "tests" ["lib/data"]

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  match hook with
  | Before_options -> Options.make_links := false
  | After_rules ->
    local_rules ();
    ocaml_lib "lib/subsocia";
    ocaml_lib "lib/data/subsocia-data"
  | _ -> ()
