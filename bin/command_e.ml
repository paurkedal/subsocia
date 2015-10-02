(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
open Command_common
open Lwt.Infix
open Subsocia_cmdliner
open Subsocia_common
open Subsocia_selector

module Entity_utils (C : Subsocia_intf.S) = struct
  include Selector_utils (C)
  include Subsocia_derived.Make (C)

  let lookup_assignment (an, vs_opt) =
    lwt Attribute_type.Ex at =
      match_lwt Attribute_type.of_name an with
      | None -> Lwt.fail (Failure ("No attribute type has name " ^ an))
      | Some at -> Lwt.return at in
    match vs_opt with
    | Some vs ->
      let t = Attribute_type.value_type at in
      let v = Value.typed_of_string t vs in
      Lwt.return (`One (C.Attribute.Ex (at, v)))
    | None ->
      Lwt.return (`All (Attribute_type.Ex at))

  let lookup_aselector (sel_opt, asgn) =
    lwt asgn = Lwt_list.map_p lookup_assignment asgn in
    lwt root = Entity.root in
    match sel_opt with
    | None ->
      Lwt.return (root, asgn)
    | Some sel ->
      lwt e_ctx = Entity.select_one sel in
      Lwt.return (e_ctx, asgn)

  let add_attributes e (e_ctx, attrs) =
    Lwt_list.iter_s
      (function
      | `One (C.Attribute.Ex (at, av)) -> Entity.add_value at av e_ctx e
      | `All _ -> assert false)
      attrs

  let delete_attributes e (e_ctx, attrs) =
    Lwt_list.iter_s
      (function
      | `One (C.Attribute.Ex (at, av)) -> Entity.remove_value at av e_ctx e
      | `All (Attribute_type.Ex at) -> Entity.clear_values at e_ctx e)
      attrs

  let entity_type_of_arg etn =
    match_lwt C.Entity_type.of_name etn with
    | None -> Lwt.fail (Failure ("No entity type has name " ^ etn))
    | Some et -> Lwt.return et
end

let ls sel = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt root = C.Entity.root in
  lwt e = U.Entity.select_one sel in
  lwt et = C.Entity.type_ e in
  lwt aus = C.Attribute_uniqueness.all () in
  let show_e ats e' =
    Lwt_list.iter_s
      (fun (C.Attribute_type.Ex at) ->
	lwt an = C.Attribute_type.name at in
	lwt vs = C.Entity.get_values at e e' >|= Values.elements in
	let vt = C.Attribute_type.value_type at in
	Lwt_list.iter_s
	  (fun av ->
	    let avr = Value.typed_to_string vt av in
	    Lwt_io.printf "{%s=%s}" an avr)
	  vs)
      ats >>
    Lwt_io.printl "" in
  let show_au au =
    lwt ats = C.Attribute_uniqueness.affected au
	  >|= C.Attribute_type.Set.elements in
    let ps =
      List.map (fun (C.Attribute_type.Ex at) -> C.Attribute.Present at) ats in
    lwt es' = C.Entity.image1 (C.Attribute.Inter ps) e in
    C.Entity.Set.iter_s (show_e ats) es' in
  C.Attribute_uniqueness.Set.iter_s show_au aus >>
  Lwt.return 0

let ls_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
		   info ~docv:"PATH" []) in
  Term.(pure ls $ sel_t)

let search sel = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt root = C.Entity.root in
  lwt es = U.Entity.select_from sel (C.Entity.Set.singleton root) in
  let show e =
    lwt name = U.Entity.display_name ~langs e in
    lwt et = C.Entity.type_ e in
    lwt etn = C.Entity_type.name et in
    Lwt_io.printlf "%s : %s" name etn >>
    lwt paths = U.Entity.paths e in
    Lwt_list.iter_s (fun p -> Lwt_io.printf "  %s\n" (string_of_selector p))
		    paths in
  C.Entity.Set.iter_s show es >>
  Lwt.return (if C.Entity.Set.is_empty es then 1 else 0)

let search_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
		   info ~docv:"PATH" []) in
  Term.(pure search $ sel_t)

let fts q etn super limit cutoff = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt root = C.Entity.root in
  lwt entity_type = Pwt_option.map_s U.entity_type_of_arg etn in
  lwt super = Pwt_option.map_s U.Entity.select_one super in
  lwt es = C.Entity.image1_fts ?entity_type ?super ?limit ?cutoff
			       (Subsocia_fts.tsquery q) root in
  let show (e, rank) =
    lwt name = U.Entity.display_name ~langs e in
    lwt et = C.Entity.type_ e in
    lwt etn = C.Entity_type.name et in
    Lwt_io.printlf "%8.3g %s : %s" rank name etn in
  Lwt_list.iter_s show es >>
  Lwt.return (if es = [] then 1 else 0)

let fts_t =
  let doc = "The query string as accepted by PostgrSQL's to_tsquery." in
  let q_t = Arg.(required & pos 0 (some string) None &
		 info ~docv:"TSQUERY" ~doc []) in
  let doc = "Restrict the result to the entities of TYPE." in
  let et_t = Arg.(value & opt (some string) None &
		  info ~docv:"TYPE" ~doc ["t"]) in
  let doc = "Restrict the result to subentities of SUPER." in
  let super_t = Arg.(value & opt (some selector_conv) None &
		     info ~docv:"SUPER" ~doc ["s"]) in
  let doc = "Only show the first LIMIT highest ranked results." in
  let limit_t = Arg.(value & opt (some int) None &
		     info ~docv:"LIMIT" ~doc ["limit"]) in
  let doc = "Exclude results rank CUTOFF and below." in
  let cutoff_t = Arg.(value & opt (some float) None &
		      info ~docv:"CUTOFF" ~doc ["cutoff"]) in
  Term.(pure fts $ q_t $ et_t $ super_t $ limit_t $ cutoff_t)

let create etn dsuper aselectors = run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt et = U.entity_type_of_arg etn in
  lwt aselectors = Lwt_list.map_p U.lookup_aselector aselectors in
  lwt dsuper = Lwt_list.map_p U.Entity.select_one dsuper in
  lwt e = C.Entity.create et in
  Lwt_list.iter_s (fun (e_sub) -> C.Entity.force_dsub e e_sub) dsuper >>
  Lwt_list.iter_s (U.add_attributes e) aselectors

let create_t =
  let etn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"TYPE" []) in
  let succs_t = Arg.(value & opt_all selector_conv [] &
		    info ~docv:"PATH" ["s"]) in
  let attrs_t = Arg.(non_empty & opt_all aselector_conv [] &
		    info ~docv:"APATH" ["a"]) in
  Term.(pure create $ etn_t $ succs_t $ attrs_t)

let delete sel = run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt e = U.Entity.select_one sel in
  C.Entity.delete e

let delete_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
		   info ~docv:"PATH" []) in
  Term.(pure delete $ sel_t)

let modify sel add_succs del_succs add_asels del_asels =
  run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt add_succs = Lwt_list.map_p U.Entity.select_one add_succs in
  lwt del_succs = Lwt_list.map_p U.Entity.select_one del_succs in
  lwt add_asels = Lwt_list.map_p U.lookup_aselector add_asels in
  lwt del_asels = Lwt_list.map_p U.lookup_aselector del_asels in
  lwt e = U.Entity.select_one sel in
  Lwt_list.iter_s (fun e_sub -> C.Entity.force_dsub e e_sub) add_succs >>
  Lwt_list.iter_s (U.add_attributes e) add_asels >>
  Lwt_list.iter_s (U.delete_attributes e) del_asels >>
  Lwt_list.iter_s (fun e_sub -> C.Entity.relax_dsub e e_sub) del_succs

let modify_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
		   info ~docv:"PATH" []) in
  let add_succs_t = Arg.(value & opt_all selector_conv [] &
			 info ~docv:"PATH" ["s"]) in
  let del_succs_t = Arg.(value & opt_all selector_conv [] &
			 info ~docv:"PATH" ["r"]) in
  let add_attrs_t = Arg.(value & opt_all aselector_conv [] &
			 info ~docv:"APATH" ["a"]) in
  let del_attrs_t = Arg.(value & opt_all aselector_pres_conv [] &
			 info ~docv:"APATH" ["d"]) in
  Term.(pure modify $ sel_t $ add_succs_t $ del_succs_t
			    $ add_attrs_t $ del_attrs_t)
