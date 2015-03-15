(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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
open Panograph_i18n
open Printf
open Subsocia_common
open Subsocia_selector
open Unprime
open Unprime_option

let connect () =
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  Subsocia_direct.connect uri

let langs = [Lang.of_string "en"] (* TODO: Use $LANG *)

let run f = Lwt_main.run (f (connect ()))
let run0 f = Lwt_main.run (f (connect ())); 0
let fail_f fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt
let invalid_arg_f fmt = ksprintf invalid_arg fmt

let value_type_parser s =
  try `Ok (Type.of_string s) with Invalid_argument msg -> `Error msg
let value_type_printer fmtr vt =
  Format.pp_print_string fmtr (Type.string_of_t0 vt)
let value_type_conv = value_type_parser, value_type_printer

let multiplicity_parser s =
  try `Ok (Multiplicity.of_string s)
  with Invalid_argument msg -> `Error msg
let multiplicity_printer fmtr mu =
  Format.pp_print_string fmtr (Multiplicity.to_string mu)
let multiplicity_conv = multiplicity_parser, multiplicity_printer

let selector_parser s =
  try `Ok (selector_of_string s)
  with Invalid_argument msg -> `Error msg
let selector_printer fmtr sel =
  Format.pp_print_string fmtr (string_of_selector sel)
let selector_conv = selector_parser, selector_printer

let aselector_parser s =
  let rec aux acc = function
    | Select_sub _ | Select_union _ | Select_pred | Select_top | Select_id _
	as sel_att ->
      invalid_arg_f "The selector %s cannot be used for attribute assignement. \
		     It must be a conjunction of one or more attribute \
		     equalities." (string_of_selector sel_att)
    | Select_inter (selA, selB) -> aux (aux acc selB) selA
    | Select_attr (an, av) -> (an, av) :: acc in
  try
    `Ok
      begin match selector_of_string s with
      | Select_sub (sel_ctx, sel_att) -> Some sel_ctx, aux [] sel_att
      | sel_att -> None, aux [] sel_att
      end
  with Invalid_argument msg -> `Error msg

let aselector_printer fmtr (ctx, asgn) =
  let sel_attr =
    match asgn with
    | [] -> assert false
    | (an, av) :: xs ->
      List.fold_left
	(fun acc (an, av) -> Select_sub (acc, Select_attr (an, av)))
	(Select_attr (an, av)) xs in
  Format.pp_print_string fmtr @@
    string_of_selector
      (match ctx with None -> sel_attr
		    | Some sel_ctx -> Select_sub (sel_ctx, sel_attr))

let aselector_conv = aselector_parser, aselector_printer

(* Database Commands *)

let schemas = ["subsocia_core.sql"; "subsocia_data.sql"]

let db_schema do_dir =
  Lwt_main.run begin
    if do_dir then
      Lwt_io.printl Subsocia_version.schema_dir
    else
      Lwt_list.iter_s
	(fun schema ->
	  Lwt_io.printl (Filename.concat Subsocia_version.schema_dir schema))
	schemas
  end; 0

let db_schema_t =
  let do_dir_t =
    Arg.(value & flag &
	 info ~doc:"Print the path to the top-level directory \
		    instead of to the individual schema files." ["dir"]) in
  Term.(pure db_schema $ do_dir_t)

let db_init () =
  (* TODO: Check presence of psql. Or better, use Caqti. Though this may
   * require splitting up the schema files into individual statements. *)
  Lwt_main.run begin
    let uri = Subsocia_config.database_uri#get in
    Pwt_list.search_s
      (fun schema ->
	let p = Filename.concat Subsocia_version.schema_dir schema in
	Lwt_log.info_f "Loading %s" p >>
	let cmd = "psql", [|"psql"; "-d"; uri; "-f"; p|] in
	match_lwt Lwt_process.exec cmd with
	| Unix.WEXITED 0 -> Lwt.return None
	| Unix.WEXITED rc -> Lwt.return (Some rc)
	| Unix.WSIGNALED sg | Unix.WSTOPPED sg ->
	  fail_f "psql received signal %d." sg)
      schemas >|= Option.get_or 0
  end

let db_init_t = Term.(pure db_init $ pure ())

(* Entity Types *)

let et_create etn = run0 @@ fun (module C) ->
  lwt et = C.Entity_type.create etn in
  Lwt_log.info_f "Created type #%ld = %s." (C.Entity_type.id et) etn

let et_create_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None &
	 info [] ~docv:"ET-NAME" ~doc:"Name of type to create.") in
  Term.(pure et_create $ et_name_t)

let et_modify etn ent_opt = run @@ fun (module C) ->
  match_lwt C.Entity_type.of_name etn with
  | None ->
    Lwt.return (`Error (false, sprintf "No type is named %s." etn))
  | Some et ->
    begin match ent_opt with
    | None -> Lwt.return_unit
    | Some ent -> C.Entity_type.set_entity_name_tmpl et ent
    end >>
    Lwt.return (`Ok 0)

let et_modify_t =
  let etn_t =
    Arg.(required & pos 0 (some string) None &
	 info ~docv:"ET-NAME" ~doc:"Name of the entity type to modify" []) in
  let ent_t =
    Arg.(value & opt (some string) None &
	 info ~docv:"TEMPLATE"
	      ~doc:"Template for the display name of entities of this type."
	      ["name-template"]) in
  Term.(ret (pure et_modify $ etn_t $ ent_t))

let et_delete etn = run @@ fun (module C) ->
  match_lwt C.Entity_type.of_name etn with
  | None ->
    Lwt.return (`Error (false, sprintf "No type is named %s." etn))
  | Some et ->
    let et_id = C.Entity_type.id et in
    C.Entity_type.delete et >>
    Lwt_log.info_f "Deleted type #%ld = %s." et_id etn >>
    Lwt.return (`Ok 0)

let et_delete_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None &
	 info ~docv:"ET-NAME" ~doc:"Name of the entity to delete" []) in
  Term.(ret (pure et_delete $ et_name_t))

let et_list () = run0 @@ fun (module C) ->
  C.Entity_type.all () >>=
  C.Entity_type.Set.iter_s
    (fun et -> C.Entity_type.name et >>= Lwt_io.printl)

let et_list_t = Term.(pure et_list $ pure ())

(* Inclusion Types *)

let in_allow etn0 etn1 = run @@ fun (module C) ->
  lwt et0 = C.Entity_type.of_name etn0 in
  lwt et1 = C.Entity_type.of_name etn1 in
  let report_missing etns =
    Lwt.return (`Error (false, "Missing types " ^ etns ^ ".")) in
  match et0, et1 with
  | Some et0, Some et1 ->
    let mu0, mu1 = Multiplicity.(May, May) in (* TODO *)
    C.Entity_type.inclusion_allow mu0 mu1 et0 et1 >>
    Lwt.return (`Ok 0)
  | None, Some _ -> report_missing etn0
  | Some _, None -> report_missing etn1
  | None, None -> report_missing (etn0 ^ " and " ^ etn1)

let in_disallow etn0 etn1 = run @@ fun (module C) ->
  lwt et0 = C.Entity_type.of_name etn0 in
  lwt et1 = C.Entity_type.of_name etn1 in
  let report_missing etns =
    Lwt.return (`Error (false, "Missing types " ^ etns ^ ".")) in
  match et0, et1 with
  | Some et0, Some et1 -> C.Entity_type.inclusion_disallow et0 et1 >>
			  Lwt.return (`Ok 0)
  | None, Some _ -> report_missing etn0
  | Some _, None -> report_missing etn1
  | None, None -> report_missing (etn0 ^ " and " ^ etn1)

let in_allow_t =
  let etn0_t = Arg.(required & pos 0 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(ret (pure in_allow $ etn0_t $ etn1_t))

let in_disallow_t =
  let etn0_t = Arg.(required & pos 0 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(ret (pure in_disallow $ etn0_t $ etn1_t))

let in_list etn0_opt etn1_opt = run @@ fun (module C) ->
  let get_et = function
    | None | Some "_" -> Lwt.return_none
    | Some etn ->
      begin match_lwt C.Entity_type.of_name etn with
      | None -> Lwt.fail (Failure ("No entity type is named " ^ etn ^ "."))
      | Some et -> Lwt.return (Some et)
      end in
  lwt et0 = get_et etn0_opt in
  lwt et1 = get_et etn1_opt in
  let pp mu0 mu1 et0 et1 =
    lwt etn0 = C.Entity_type.name et0 in
    lwt etn1 = C.Entity_type.name et1 in
    Lwt_io.printlf "%30s %s%s %-30s"
      etn0 (Multiplicity.to_string mu0) (Multiplicity.to_string mu1) etn1 in
  match et0, et1 with
  | None, None ->
    C.Entity_type.inclusion_dump () >>=
      Lwt_list.iter_s (fun (et0, et1, mu0, mu1) -> pp mu0 mu1 et0 et1) >>
    Lwt.return 0
  | Some et0, None ->
    C.Entity_type.inclusion_succs et0 >>=
      C.Entity_type.Map.iter_s (fun et1 (mu0, mu1) -> pp mu0 mu1 et0 et1) >>
    Lwt.return 0
  | None, Some et1 ->
    C.Entity_type.inclusion_preds et1 >>=
      C.Entity_type.Map.iter_s (fun et0 (mu0, mu1) -> pp mu0 mu1 et0 et1) >>
    Lwt.return 0
  | Some et0, Some et1 ->
    begin match_lwt C.Entity_type.inclusion et0 et1 with
    | Some (mu0, mu1) ->
      Lwt_io.printlf "%s%s" (Multiplicity.to_string mu0)
			    (Multiplicity.to_string mu1) >>
      Lwt.return 0
    | None ->
      Lwt.return 1
    end

let in_list_t =
  let etn0_t = Arg.(value & pos 0 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(value & pos 1 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(pure in_list $ etn0_t $ etn1_t)

(* Attributes *)

let at_create vt atn = run0 @@ fun (module C) ->
  C.Attribute_type.create vt atn >>= fun at ->
  Lwt_log.info_f "Created attribute type #%ld %s." (C.Attribute_type.id at) atn

let at_create_t =
  let vt_t = Arg.(required & pos 1 (some value_type_conv) None &
		  info ~docv:"TYPE" []) in
  let atn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"NAME" []) in
  Term.(pure at_create $ vt_t $ atn_t)

let at_delete atn = run @@ fun (module C) ->
  match_lwt C.Attribute_type.of_name atn with
  | Some at ->
    C.Attribute_type.delete at >>
    Lwt_log.info_f "Delete attribute type #%ld %s."
		   (C.Attribute_type.id at) atn >>
    Lwt.return 0
  | None ->
    Lwt_log.error_f "No attribute type is named %s." atn >>
    Lwt.return 1

let at_delete_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"NAME" []) in
  Term.(pure at_delete $ atn_t)

let req what name = function
  | None -> Lwt.fail (Failure ("There is no " ^ what ^ " named " ^ name ^ "."))
  | Some x -> Lwt.return x

let an_allow atn etn0 etn1 mu = run0 @@ fun (module C) ->
  lwt at = C.Attribute_type.of_name atn >>= req "attribute type" atn in
  lwt et0 = C.Entity_type.of_name etn0 >>= req "entity type" etn0 in
  lwt et1 = C.Entity_type.of_name etn1 >>= req "entity type" etn1 in
  C.Entity_type.attribution_allow et0 et1 at mu

let an_allow_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"NAME" []) in
  let etn0_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 2 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  let mu = Arg.(value & pos 3 multiplicity_conv Multiplicity.May1 &
		info ~docv:"MULTIPLICITY" []) in
  Term.(pure an_allow $ atn_t $ etn0_t $ etn1_t $ mu)

let an_disallow atn etn0 etn1 = run0 @@ fun (module C) ->
  lwt at = C.Attribute_type.of_name atn >>= req "attribute type" atn in
  lwt et0 = C.Entity_type.of_name etn0 >>= req "entity type" etn0 in
  lwt et1 = C.Entity_type.of_name etn1 >>= req "entity type" etn1 in
  C.Entity_type.attribution_disallow et0 et1 at

let an_disallow_t =
  let atn_t = Arg.(required & pos 0 (some string) None &
		   info ~docv:"NAME" []) in
  let etn0_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 2 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(pure an_disallow $ atn_t $ etn0_t $ etn1_t)

let an_list () = run0 @@ fun (module C) ->
  C.Entity_type.attribution_dump () >>=
  Lwt_list.map_s @@ fun (et0, et1, at, mu) ->
  lwt atn = C.Attribute_type.name at in
  lwt etn0 = C.Entity_type.name et0 in
  lwt etn1 = C.Entity_type.name et1 in
  Lwt_io.printlf "%s %s %s %s" (Multiplicity.to_string mu) atn etn0 etn1

let an_list_t = Term.(pure an_list $ pure ())

(* Entities *)

module Entity_utils (C : Subsocia_intf.S) = struct
  include Selector_utils (C)
  include Subsocia_derived.Make (C)

  let lookup_assignment (an, vs) =
    lwt Attribute_type.Ex at =
      match_lwt Attribute_type.of_name an with
      | None -> Lwt.fail (Failure ("No attribute type has name " ^ an))
      | Some at -> Lwt.return at in
    let t = Attribute_type.type1 at in
    let v = Value.typed_of_string t vs in
    Lwt.return (C.Attribute.Ex (at, v))

  let lookup_aselector (sel_opt, asgn) =
    lwt asgn = Lwt_list.map_p lookup_assignment asgn in
    lwt e_top = Entity.top in
    match sel_opt with
    | None ->
      Lwt.return (e_top, asgn)
    | Some sel ->
      lwt e_ctx = Entity.select_one sel in
      Lwt.return (e_ctx, asgn)

  let add_attributes e (e_ctx, attrs) =
    Lwt_list.iter_s
      (fun (C.Attribute.Ex (at, av)) ->
	Entity.precedes e e_ctx >>=
	  (function
	    | true -> Lwt.return_unit
	    | false ->
	      lwt ctx_name = Entity.display_name ~langs e_ctx in
	      Lwt_log.info_f "Adding required inclusion under %s." ctx_name >>
	      Entity.constrain e e_ctx) >>
	Entity.setattr e e_ctx at [av])
      attrs

  let delete_attributes e (e_ctx, attrs) =
    Lwt_list.iter_s
      (fun (C.Attribute.Ex (at, av)) -> Entity.delattr e e_ctx at [av])
      attrs
end

let search sel = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt e_top = C.Entity.top in
  lwt es = U.Entity.select_from sel (C.Entity.Set.singleton e_top) in
  let show e =
    lwt name = U.Entity.display_name ~langs e in
    lwt et = C.Entity.type_ e in
    lwt etn = C.Entity_type.name et in
    Lwt_io.printlf "%s : %s" name etn in
  C.Entity.Set.iter_s show es >>
  Lwt.return (if C.Entity.Set.is_empty es then 1 else 0)

let search_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
		   info ~docv:"PATH" []) in
  Term.(pure search $ sel_t)

let create etn succs aselectors = run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt et =
    match_lwt C.Entity_type.of_name etn with
    | None -> Lwt.fail (Failure ("No entity type has name " ^ etn))
    | Some et -> Lwt.return et in
  lwt viewer = U.Const.e_default_viewers in
  lwt admin = U.Const.e_default_admins in
  lwt aselectors = Lwt_list.map_p U.lookup_aselector aselectors in
  lwt succs = Lwt_list.map_p U.Entity.select_one succs in
  lwt e = C.Entity.create ~viewer ~admin et in
  Lwt_list.iter_s (fun (e_sub) -> C.Entity.constrain e e_sub) succs >>
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

let modify sel add_succs del_succs add_asels del_asels admin viewer =
  run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  lwt add_succs = Lwt_list.map_p U.Entity.select_one add_succs in
  lwt del_succs = Lwt_list.map_p U.Entity.select_one del_succs in
  lwt add_asels = Lwt_list.map_p U.lookup_aselector add_asels in
  lwt del_asels = Lwt_list.map_p U.lookup_aselector del_asels in
  lwt admin = Pwt_option.map_s U.Entity.select_one admin in
  lwt viewer = Pwt_option.map_s U.Entity.select_one viewer in
  lwt e = U.Entity.select_one sel in
  Lwt_list.iter_s (fun e_sub -> C.Entity.constrain e e_sub) add_succs >>
  Lwt_list.iter_s (U.add_attributes e) add_asels >>
  Lwt_list.iter_s (U.delete_attributes e) del_asels >>
  Lwt_list.iter_s (fun e_sub -> C.Entity.unconstrain e e_sub) del_succs >>
  if admin <> None || viewer <> None then
    C.Entity.modify ?admin ?viewer e
  else
    Lwt.return_unit

let modify_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
		   info ~docv:"PATH" []) in
  let add_succs_t = Arg.(value & opt_all selector_conv [] &
			 info ~docv:"PATH" ["s"]) in
  let del_succs_t = Arg.(value & opt_all selector_conv [] &
			 info ~docv:"PATH" ["r"]) in
  let add_attrs_t = Arg.(value & opt_all aselector_conv [] &
			 info ~docv:"APATH" ["a"]) in
  let del_attrs_t = Arg.(value & opt_all aselector_conv [] &
			 info ~docv:"APATH" ["d"]) in
  let admin_t = Arg.(value & opt (some selector_conv) None &
		     info ~docv:"PATH" ~doc:"Set administrator group."
			  ["admin"]) in
  let viewer_t = Arg.(value & opt (some selector_conv) None &
		      info ~docv:"PATH" ~doc:"Set viewer group."
			   ["viewer"]) in
  Term.(pure modify $ sel_t $ add_succs_t $ del_succs_t
			    $ add_attrs_t $ del_attrs_t
			    $ admin_t $ viewer_t)

(* Main *)

let db_scn = "DATABASE COMMANDS"
let et_scn = "ENTITY TYPE COMMANDS"
let in_scn = "INCLUSION COMMANDS"
let at_scn = "ATTRIBUTE TYPE COMMANDS"
let an_scn = "ATTRIBUTION COMMANDS"
let e_scn = "ENTITY COMMANDS"

let subcommands = [
  db_schema_t, Term.info ~docs:db_scn
    ~doc:"Print the directory or paths of database schema files."
    "db-schema";
  db_init_t, Term.info ~docs:db_scn
    ~doc:"Initialize the database."
    "db-init";
  et_list_t, Term.info ~docs:et_scn
    ~doc:"List entity types."
    "et-list";
  et_create_t, Term.info ~docs:et_scn
    ~doc:"Create an entity type."
    "et-create";
  et_modify_t, Term.info ~docs:et_scn
    ~doc:"Modify an entity type."
    "et-modify";
  et_delete_t, Term.info ~docs:et_scn
    ~doc:"Delete an entity type."
    "et-delete";
  in_allow_t, Term.info ~docs:in_scn
    ~doc:"Allow inclusion between entities of a type."
    "in-allow";
  in_disallow_t, Term.info ~docs:in_scn
    ~doc:"Disallow inclusion between entities of a type."
    "in-disallow";
  in_list_t, Term.info ~docs:in_scn
    ~doc:"Show inclusion policy between types."
    "in-list";
  at_create_t, Term.info ~docs:at_scn
    ~doc:"Create an attribute type."
    "at-create";
  at_delete_t, Term.info ~docs:at_scn
    ~doc:"Delete an attribute type."
    "at-delete";
  an_allow_t, Term.info ~docs:an_scn
    ~doc:"Allow an attribution."
    "an-allow";
  an_disallow_t, Term.info ~docs:an_scn
    ~doc:"Disallow an attribution."
    "an-disallow";
  an_list_t, Term.info ~docs:an_scn
    ~doc:"List allowed attribution."
    "an-list";
  search_t, Term.info ~docs:e_scn
    ~doc:"List entities matching a selector."
    "search";
  create_t, Term.info ~docs:e_scn
    ~doc:"Create an entity."
    "create";
  delete_t, Term.info ~docs:e_scn
    ~doc:"Delete an entity."
    "delete";
  modify_t, Term.info ~docs:e_scn
    ~doc:"Modify an entity."
    "modify";
]

let main_t = Term.(ret @@ pure (`Error (true, "Missing subcommand.")))

let () =
  match Term.eval_choice (main_t, Term.info "subsocia") subcommands with
  | `Error `Parse -> exit 64
  | `Error `Term -> exit 69
  | `Error `Exn -> exit 70
  | `Ok rc -> exit rc
  | `Version | `Help -> exit 0
