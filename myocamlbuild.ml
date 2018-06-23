open Ocamlbuild_plugin

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

let local_rules () =
  sed_rule ~dep:"lib/subsocia_version.ml.in" ~prod:"lib/subsocia_version.ml"
    ["s;@SUBSOCIA_DATADIR@;" ^ subsocia_datadir ^ ";g"];

  rule "%.mli & %.idem -> %.ml"
    ~deps:["%.mli"; "%.idem"] ~prod:"%.ml"
    begin fun env build ->
      let src = env "%.mli" and dst = env "%.ml" in
      cp src dst
    end;
  Pathname.define_context "bin" ["lib-data"];
  Pathname.define_context "tests" ["lib-data"]

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  (match hook with
   | Before_options -> Options.make_links := false
   | After_rules ->
      local_rules ();
      ocaml_lib "lib/subsocia";
      ocaml_lib "lib-data/subsocia-data";
      flag ["ocaml"; "compile"] (S[A"-w"; A"+A-4-42-44-48"]);
      (match Sys.getenv "TERM" with
       | exception Not_found -> ()
       | "" | "dumb" -> ()
       | _ -> flag ["ocaml"; "compile"] (S [A"-color"; A"always"]))
   | _ -> ())
