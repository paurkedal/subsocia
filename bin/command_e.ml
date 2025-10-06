(* Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

open Cmdliner
open Command_common
open Lwt.Infix
open Lwt.Syntax
open Subsocia_cmdliner
open Subsocia_common
open Subsocia_prereq
open Subsocia_selector
open Subsocia_selector_types
open Unprime
open Unprime_list
open Unprime_option

let docs = "ENTITY COMMANDS"

type entity_details_selection = {
  eds_paths : bool;
  eds_super : bool;
  eds_sub : bool;
  eds_history : bool;
}

let eds_default = {
  eds_paths = false;
  eds_super = false;
  eds_sub = false;
  eds_history = false;
}

let eds_conv =
  let parse s =
    let paths = ref false in
    let super = ref false in
    let sub = ref false in
    let history = ref false in
    let aux = function
      | "all" -> paths := true; super := true; sub := true
      | "paths" -> paths := true
      | "super" -> super := true
      | "sub" -> sub := true
      | "history" -> history := true
      | _ -> failwith "Invalid display option." in
    try
      s |> Prime_string.chop_affix "," |> List.iter aux;
      `Ok {
        eds_paths = !paths;
        eds_super = !super;
        eds_sub = !sub;
        eds_history = !history;
      }
    with Failure msg ->
      `Error msg
  in
  let print fo eds =
    [] |> (if eds.eds_paths then List.cons "paths" else ident)
       |> (if eds.eds_super then List.cons "super" else ident)
       |> (if eds.eds_sub then List.cons "sub" else ident)
       |> (if eds.eds_history then List.cons "history" else ident)
       |> String.concat "," |> Format.pp_print_string fo
  in
  (parse, print)

module Entity_utils (C : Subsocia_intf.S) = struct
  include Selector_utils (C)
  include Subsocia_derived.Make (C)

  type attribute_update =
    | Add_value : 'a Attribute_type.t * 'a -> attribute_update
    | Remove_value : 'a Attribute_type.t * 'a -> attribute_update
    | Clear_values : 'a Attribute_type.t -> attribute_update

  let update_attributes e (e_ctx, updates) =
    let aux = function
      | Add_value (at, av) -> Entity.add_value at av e_ctx e
      | Remove_value (at, av) -> Entity.remove_value at av e_ctx e
      | Clear_values at -> Entity.clear_values at e_ctx e
    in
    Lwt_list.iter_s aux updates

  let lookup_add_selector ?time (ctx, asgn) =
    let aux (an, avs_str) =
      let* Attribute_type.Any at = Attribute_type.any_of_name_exn an in
      let t = Attribute_type.value_type at in
      Lwt.return @@ List.map
        (fun av_str -> Add_value (at, Value.typed_of_string t av_str))
        avs_str
    in
    let* asgn =
      List.flatten =|< Lwt_list.map_p aux (String_map.bindings asgn)
    in
    let* root = Entity.get_root () in
    (match ctx with
     | None -> Lwt.return (root, asgn)
     | Some ctx -> Entity.select_one ?time ctx >|= fun e_ctx -> (e_ctx, asgn))

  let lookup_delete_selector ?time (ctx, asgn) =
    let aux (an, avs_str) =
      let* Attribute_type.Any at = Attribute_type.any_of_name_exn an in
      let t = Attribute_type.value_type at in
      match avs_str with
      | Some avs_str ->
        Lwt.return @@ List.map
          (fun av_str -> Remove_value (at, Value.typed_of_string t av_str))
          avs_str
      | None -> Lwt.return [Clear_values at]
    in
    let* asgn =
      List.flatten =|< Lwt_list.map_p aux (String_map.bindings asgn)
    in
    let* root = Entity.get_root () in
    (match ctx with
     | None -> Lwt.return (root, asgn)
     | Some ctx -> Entity.select_one ?time ctx >|= fun e_ctx -> (e_ctx, asgn))

  let entity_type_of_arg etn =
    C.Entity_type.of_name etn >>= function
     | None -> Lwt.fail (Failure ("No entity type has name " ^ etn))
     | Some et -> Lwt.return et

  let show_entity_list pfx =
    Entity.Set.iter_s begin fun e ->
      Lwt_io.print pfx >>= fun () ->
      (Entity.paths e >>= function
       | [] -> Entity.display_name e
       | p :: _ -> Lwt.return (string_of_selector p))
      >>= Lwt_io.printl
    end

  let show_history pfx =
    Lwt_list.iter_s begin fun (since, until, e) ->
      let since_str = Ptime.to_rfc3339 ~tz_offset_s:0 since in
      let until_str =
        (match until with
         | None -> "∞"
         | Some until -> Ptime.to_rfc3339 ~tz_offset_s:0 until)
      in
      Lwt_io.printf "%s[%s, %s) " pfx since_str until_str >>= fun () ->
      (Entity.paths e >>= function
       | [] -> Entity.display_name e
       | p :: _ -> Lwt.return (string_of_selector p))
      >>= Lwt_io.printl
    end

  let show_entity ?time eds e =
    let* name = Entity.display_name ~langs e in
    let* et = C.Entity.entity_type e in
    let* etn = C.Entity_type.name et in
    let* e_idstr = Entity.soid_string e in
    Lwt_io.printlf "%s %s : %s" e_idstr name etn >>= fun () ->
    ( if not eds.eds_paths then Lwt.return_unit else
      let* paths = Entity.paths e in
      Lwt_list.iter_s
        (fun p -> Lwt_io.printf "  = %s\n" (string_of_selector p))
        paths ) >>= fun () ->
    ( if not eds.eds_super then Lwt.return_unit else
      if eds.eds_history then
        C.Entity.dsuper_history e >>= show_history "  ⊂ "
      else
        C.Entity.dsuper ?time e >>= show_entity_list "  ⊂ " ) >>= fun () ->
    ( if not eds.eds_sub then Lwt.return_unit else
      if eds.eds_history then
        C.Entity.dsub_history e >>= show_history "  ⊃ "
      else
        C.Entity.dsub ?time e >>= show_entity_list "  ⊃ " )
end

let e_ls sel_opt time =
  run_exn @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let sel = Option.get_or Select_root sel_opt in
  let* e = U.Entity.select_one ?time sel in
  let* aus = C.Attribute_uniqueness.all () in
  let show_e ats e' =
    Lwt_list.iter_s
      (fun (C.Attribute_type.Any at) ->
        let* an = C.Attribute_type.name at in
        let* vs = C.Entity.get_values at e e' >|= Values.elements in
        let vt = C.Attribute_type.value_type at in
        Lwt_list.iter_s
          (fun av ->
            let avr = Value.typed_to_string vt av in
            Lwt_io.printf "{%s=%s}" an avr)
          vs)
      ats >>= fun () ->
    Lwt_io.printl ""
  in
  let show_au au =
    let* ats =
      C.Attribute_uniqueness.affected au >|= C.Attribute_type.Set.elements in
    let ps =
      List.map (fun (C.Attribute_type.Any at) -> C.Relation.Present at) ats in
    let* es' = C.Entity.image1 (C.Relation.Inter ps) e in
    C.Entity.Set.iter_s (show_e ats) es'
  in
  C.Attribute_uniqueness.Set.iter_s show_au aus

let e_ls_cmd =
  let sel =
    Arg.(value & pos 0 (some selector) None & info ~docv:"PATH" [])
  in
  let time =
    let doc = "Time at which to traverse inclusion. Defaults to now." in
    Arg.(value & opt (some ptime) None & info ~docv:"QUERY-TIME" ~doc ["time"])
  in
  let term = Term.(const e_ls $ sel $ time) in
  let info = Cmd.info ~docs ~doc:"List entities reachable from a path." "ls" in
  Cmd.v info (with_log term)

let e_search sel eds time =
  run_bool_exn @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let* root = C.Entity.get_root () in
  let* es = U.Entity.select_from ?time sel (C.Entity.Set.singleton root) in
  C.Entity.Set.iter_s (U.show_entity ?time eds) es >>= fun () ->
  Lwt.return (not (C.Entity.Set.is_empty es))

let e_search_cmd =
  let sel =
    Arg.(required & pos 0 (some selector) None & info ~docv:"PATH" [])
  in
  let eds =
    let docv = "COMMA-SEPARATED-LIST" in
    let doc =
      "Extra information to show for each entity: \
       `all', `paths', `super', `sub', and `history', \
       where `history' is combined with `sub' and `super' to show the full \
       inclusion history with times of validity."
    in
    Arg.(value & opt eds_conv eds_default & info ~docv ~doc ["D"])
  in
  let time =
    let doc = "Time at which to traverse inclusion. Defaults to now." in
    Arg.(value & opt (some ptime) None & info ~docv:"QUERY-TIME" ~doc ["time"])
  in
  let term = Term.(const e_search $ sel $ eds $ time) in
  let info =
    Cmd.info ~docs ~doc:"List entities matching a selector." "search"
  in
  Cmd.v info (with_log term)

let e_fts q etn super limit cutoff time =
  run_bool_exn @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let* root = C.Entity.get_root () in
  let* entity_type = Lwt_option.map_s U.entity_type_of_arg etn in
  let* super = Lwt_option.map_s (U.Entity.select_one ?time) super in
  let* es =
    C.Entity.image1_fts ?entity_type ?super ?limit ?cutoff
      (Subsocia_fts.tsquery q) root
  in
  let show (e, rank) =
    let* name = U.Entity.display_name ~langs e in
    let* et = C.Entity.entity_type e in
    let* etn = C.Entity_type.name et in
    Lwt_io.printlf "%8.3g %s : %s" rank name etn
  in
  Lwt_list.iter_s show es >>= fun () ->
  Lwt.return (es <> [])

let e_fts_cmd =
  let query =
    let doc = "The query string as accepted by PostgrSQL's to_tsquery." in
    Arg.(required & pos 0 (some string) None & info ~docv:"TSQUERY" ~doc [])
  in
  let et =
    let doc = "Restrict the result to the entities of TYPE." in
    Arg.(value & opt (some string) None & info ~docv:"TYPE" ~doc ["t"; "type"])
  in
  let super =
    let doc = "Restrict the result to subentities of SUPER." in
    Arg.(value & opt (some selector) None & info ~docv:"SUPER" ~doc ["s"])
  in
  let limit =
    let doc = "Only show the first LIMIT highest ranked results." in
    Arg.(value & opt (some int) None & info ~docv:"LIMIT" ~doc ["limit"])
  in
  let cutoff =
    let doc = "Exclude results rank CUTOFF and below." in
    Arg.(value & opt (some float) None & info ~docv:"CUTOFF" ~doc ["cutoff"])
  in
  let time =
    let doc = "Time at which to traverse inclusion. Defaults to now." in
    Arg.(value & opt (some ptime) None & info ~docv:"QUERY-TIME" ~doc ["time"])
  in
  let term = Term.(const e_fts $ query $ et $ super $ limit $ cutoff $ time) in
  let info = Cmd.info ~docs ~doc:"Full-text search." "fts" in
  Cmd.v info (with_log term)

let e_create etn add_dsupers add_sels time =
  run_exn @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let* et = U.entity_type_of_arg etn in
  let* add_sels = Lwt_list.map_p U.lookup_add_selector add_sels in
  let* add_dsupers =
    Lwt_list.map_p (U.Entity.select_one ?time) add_dsupers
  in
  let* e = C.Entity.create et in
  Lwt_list.iter_s (C.Entity.force_dsub ?time e) add_dsupers >>= fun () ->
  Lwt_list.iter_s (U.update_attributes e) add_sels

let e_create_cmd =
  let etn =
    Arg.(required & pos 0 (some string) None & info ~docv:"TYPE" [])
  in
  let succs =
    Arg.(value & opt_all selector [] & info ~docv:"PATH" ["s"])
  in
  let attrs =
    Arg.(non_empty & opt_all add_selector [] & info ~docv:"APATH" ["a"])
  in
  let time =
    let doc =
      "Time at which to enforce and traverse inclusions. Defaults to now."
    in
    Arg.(value & opt (some ptime) None & info ~docv:"TIME" ~doc ["time"])
  in
  let term = Term.(const e_create $ etn $ succs $ attrs $ time) in
  let info = Cmd.info ~docs ~doc:"Create an entity." "create" in
  Cmd.v info (with_log term)

let e_delete sel del_dsupers del_sels time =
  run_exn @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let* e = U.Entity.select_one ?time sel in
  let* del_dsupers = Lwt_list.map_p (U.Entity.select_one ?time) del_dsupers in
  let* del_sels = Lwt_list.map_p (U.lookup_delete_selector ?time) del_sels in
  let* () = Lwt_list.iter_s (U.update_attributes e) del_sels in
  let* () = Lwt_list.iter_s (C.Entity.relax_dsub ?time e) del_dsupers in
  C.Entity.delete e

let e_delete_cmd =
  let sel =
    Arg.(required & pos 0 (some selector) None & info ~docv:"PATH" [])
  in
  let del_dsupers =
    let doc = "Remove the inclusion of this entity in SUPER." in
    Arg.(value & opt_all selector [] & info ~docv:"SUPER" ~doc ["r"])
  in
  let del_attrs =
    let doc =
      "Remove the arrow which is \
       labelled by final component of APATH, \
       have the leading components as source, and \
       have the current entry as target."
    in
    Arg.(value & opt_all delete_selector [] & info ~docv:"APATH" ~doc ["d"])
  in
  let time =
    let doc = "Time at which to traverse inclusion. Defaults to now." in
    Arg.(value & opt (some ptime) None & info ~docv:"QUERY-TIME" ~doc ["time"])
  in
  let term = Term.(const e_delete $ sel $ del_dsupers $ del_attrs $ time) in
  let info = Cmd.info ~docs ~doc:"Delete an entity." "delete" in
  Cmd.v info (with_log term)

let e_modify sel add_dsupers del_dsupers add_sels del_sels time =
  run_exn @@ fun (module C) ->
  C.transaction @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let* add_dsupers = Lwt_list.map_p (U.Entity.select_one ?time) add_dsupers in
  let* del_dsupers = Lwt_list.map_p (U.Entity.select_one ?time) del_dsupers in
  let* add_sels = Lwt_list.map_p (U.lookup_add_selector ?time) add_sels in
  let* del_sels = Lwt_list.map_p (U.lookup_delete_selector ?time) del_sels in
  let* e = U.Entity.select_one ?time sel in
  Lwt_list.iter_s (fun e_sub -> C.Entity.force_dsub ?time e e_sub) add_dsupers
    >>= fun () ->
  Lwt_list.iter_s (U.update_attributes e) add_sels >>= fun () ->
  Lwt_list.iter_s (U.update_attributes e) del_sels >>= fun () ->
  Lwt_list.iter_s (fun e_sub -> C.Entity.relax_dsub ?time e e_sub) del_dsupers

let e_modify_cmd =
  let sel =
    let doc =
      "The entity to modify. For the purpose of this utility, \
       additions and removals of arrows are considered to change \
       the target entity and \
       additions and removals of inclusions are considered to change \
       the subentity."
    in
    Arg.(required & pos 0 (some selector) None & info ~docv:"PATH" ~doc [])
  in
  let add_dsupers =
    let doc = "Add an inclusion of this entity in SUPER." in
    Arg.(value & opt_all selector [] & info ~docv:"SUPER" ~doc ["s"])
  in
  let del_dsupers =
    let doc = "Remove the inclusion of this entity in SUPER." in
    Arg.(value & opt_all selector [] & info ~docv:"SUPER" ~doc ["r"])
  in
  let add_attrs =
    let doc =
      "Add an arrow which is \
       labelled by final component of APATH, \
       have the leading components as source, and \
       have the current entry as target."
    in
    Arg.(value & opt_all add_selector [] & info ~docv:"APATH" ~doc ["a"])
  in
  let del_attrs =
    let doc =
      "Remove the arrow which is \
       labelled by final component of APATH, \
       have the leading components as source, and \
       have the current entry as target."
    in
    Arg.(value & opt_all delete_selector [] & info ~docv:"APATH" ~doc ["d"])
  in
  let time =
    let doc =
      "Start or end of validity to record for added and removed inclusions. \
       Any inclusions needed by input paths are also probed at this time. \
       Defaults to now."
    in
    Arg.(value & opt (some ptime) None & info ~docv:"TIME" ~doc ["time"])
  in
  let term = let open Term in
    const e_modify
      $ sel
      $ add_dsupers $ del_dsupers
      $ add_attrs $ del_attrs
      $ time
  in
  let info = Cmd.info ~docs ~doc:"Modify an entity." "modify" in
  Cmd.v info (with_log term)
