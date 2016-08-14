(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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
open Subsocia_selector_types
open Unprime
open Unprime_list
open Unprime_option

type entity_details_selection = {
  eds_paths : bool;
  eds_super : bool;
  eds_sub : bool;
}

let eds_default = {
  eds_paths = false;
  eds_super = false;
  eds_sub = false;
}

let eds_conv =
  let parse s =
    let paths = ref false in
    let super = ref false in
    let sub = ref false in
    let aux = function
      | "all" -> paths := true; super := true; sub := true
      | "paths" -> paths := true
      | "super" -> super := true
      | "sub" -> sub := true
      | _ -> failwith "Invalid display option." in
    try
      s |> Prime_string.chop_affix "," |> List.iter aux;
      `Ok { eds_paths = !paths; eds_super = !super; eds_sub = !sub }
    with Failure msg ->
      `Error msg in
  let print fo eds =
    [] |> (if eds.eds_paths then List.push "paths" else ident)
       |> (if eds.eds_super then List.push "super" else ident)
       |> (if eds.eds_sub then List.push "sub" else ident)
       |> String.concat "," |> Format.pp_print_string fo in
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
      | Clear_values at -> Entity.clear_values at e_ctx e in
    Lwt_list.iter_s aux updates

  let lookup_add_selector (ctx, asgn) =
    let aux (an, avs_str) =
      let%lwt Attribute_type.Ex at = Attribute_type.required an in
      let t = Attribute_type.value_type at in
      Lwt.return @@ List.map
        (fun av_str -> Add_value (at, Value.typed_of_string t av_str))
        avs_str in
    let%lwt asgn =
      List.flatten =|< Lwt_list.map_p aux (String_map.bindings asgn) in
    let%lwt root = Entity.root in
    match ctx with
    | None -> Lwt.return (root, asgn)
    | Some ctx -> Entity.select_one ctx >|= fun e_ctx -> (e_ctx, asgn)

  let lookup_delete_selector (ctx, asgn) =
    let aux (an, avs_str) =
      let%lwt Attribute_type.Ex at = Attribute_type.required an in
      let t = Attribute_type.value_type at in
      match avs_str with
      | Some avs_str ->
        Lwt.return @@ List.map
          (fun av_str -> Remove_value (at, Value.typed_of_string t av_str))
          avs_str
      | None -> Lwt.return [Clear_values at] in
    let%lwt asgn =
      List.flatten =|< Lwt_list.map_p aux (String_map.bindings asgn) in
    let%lwt root = Entity.root in
    match ctx with
    | None -> Lwt.return (root, asgn)
    | Some ctx -> Entity.select_one ctx >|= fun e_ctx -> (e_ctx, asgn)

  let entity_type_of_arg etn =
    match%lwt C.Entity_type.of_name etn with
    | None -> Lwt.fail (Failure ("No entity type has name " ^ etn))
    | Some et -> Lwt.return et

  let show_entity_list pfx =
    Entity.Set.iter_s begin fun e ->
      Lwt_io.print pfx >>
      begin match%lwt Entity.paths e with
      | [] -> Entity.display_name e
      | p :: _ -> Lwt.return (string_of_selector p)
      end >>= Lwt_io.printl
    end

  let show_entity eds e =
    let%lwt name = Entity.display_name ~langs e in
    let%lwt et = C.Entity.entity_type e in
    let%lwt etn = C.Entity_type.name et in
    let%lwt e_idstr = Entity.soid_string e in
    Lwt_io.printlf "%s %s : %s" e_idstr name etn >>
    ( if not eds.eds_paths then Lwt.return_unit else
      let%lwt paths = Entity.paths e in
      Lwt_list.iter_s (fun p -> Lwt_io.printf "  = %s\n" (string_of_selector p))
                      paths ) >>
    ( if not eds.eds_super then Lwt.return_unit else
      C.Entity.dsuper e >>= show_entity_list "  ⊂ " ) >>
    ( if not eds.eds_sub then Lwt.return_unit else
      C.Entity.dsub e >>= show_entity_list "  ⊃ " )

end

let e_ls sel_opt = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let sel = Option.get_or Select_root sel_opt in
  let%lwt root = C.Entity.root in
  let%lwt e = U.Entity.select_one sel in
  let%lwt et = C.Entity.entity_type e in
  let%lwt aus = C.Attribute_uniqueness.all () in
  let show_e ats e' =
    Lwt_list.iter_s
      (fun (C.Attribute_type.Ex at) ->
        let%lwt an = C.Attribute_type.name at in
        let%lwt vs = C.Entity.get_values at e e' >|= Values.elements in
        let vt = C.Attribute_type.value_type at in
        Lwt_list.iter_s
          (fun av ->
            let avr = Value.typed_to_string vt av in
            Lwt_io.printf "{%s=%s}" an avr)
          vs)
      ats >>
    Lwt_io.printl "" in
  let show_au au =
    let%lwt ats =
      C.Attribute_uniqueness.affected au >|= C.Attribute_type.Set.elements in
    let ps =
      List.map (fun (C.Attribute_type.Ex at) -> C.Relation.Present at) ats in
    let%lwt es' = C.Entity.image1 (C.Relation.Inter ps) e in
    C.Entity.Set.iter_s (show_e ats) es' in
  C.Attribute_uniqueness.Set.iter_s show_au aus >>
  Lwt.return 0

let e_ls_t =
  let sel_t = Arg.(value & pos 0 (some selector_conv) None &
                   info ~docv:"PATH" []) in
  Term.(pure e_ls $ sel_t)

let e_search sel eds = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let%lwt root = C.Entity.root in
  let%lwt es = U.Entity.select_from sel (C.Entity.Set.singleton root) in
  C.Entity.Set.iter_s (U.show_entity eds) es >>
  Lwt.return (if C.Entity.Set.is_empty es then 1 else 0)

let e_search_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
                   info ~docv:"PATH" []) in
  let doc =
    "Extra information to show for each entity: all, paths, super, sub" in
  let eds_t = Arg.(value & opt eds_conv eds_default &
                   info ~docv:"COMMA-SEPARATED-LIST" ~doc ["D"]) in
  Term.(pure e_search $ sel_t $ eds_t)

let e_fts q etn super limit cutoff = run @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let%lwt root = C.Entity.root in
  let%lwt entity_type = Pwt_option.map_s U.entity_type_of_arg etn in
  let%lwt super = Pwt_option.map_s U.Entity.select_one super in
  let%lwt es = C.Entity.image1_fts ?entity_type ?super ?limit ?cutoff
                                   (Subsocia_fts.tsquery q) root in
  let show (e, rank) =
    let%lwt name = U.Entity.display_name ~langs e in
    let%lwt et = C.Entity.entity_type e in
    let%lwt etn = C.Entity_type.name et in
    Lwt_io.printlf "%8.3g %s : %s" rank name etn in
  Lwt_list.iter_s show es >>
  Lwt.return (if es = [] then 1 else 0)

let e_fts_t =
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
  Term.(pure e_fts $ q_t $ et_t $ super_t $ limit_t $ cutoff_t)

let e_create etn add_dsupers add_sels = run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let%lwt et = U.entity_type_of_arg etn in
  let%lwt add_sels = Lwt_list.map_p U.lookup_add_selector add_sels in
  let%lwt add_dsupers = Lwt_list.map_p U.Entity.select_one add_dsupers in
  let%lwt e = C.Entity.create et in
  Lwt_list.iter_s (C.Entity.force_dsub e) add_dsupers >>
  Lwt_list.iter_s (U.update_attributes e) add_sels

let e_create_t =
  let etn_t = Arg.(required & pos 0 (some string) None &
                   info ~docv:"TYPE" []) in
  let succs_t = Arg.(value & opt_all selector_conv [] &
                    info ~docv:"PATH" ["s"]) in
  let attrs_t = Arg.(non_empty & opt_all add_selector_conv [] &
                    info ~docv:"APATH" ["a"]) in
  Term.(pure e_create $ etn_t $ succs_t $ attrs_t)

let e_delete sel = run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let%lwt e = U.Entity.select_one sel in
  C.Entity.delete e

let e_delete_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
                   info ~docv:"PATH" []) in
  Term.(pure e_delete $ sel_t)

let e_modify sel add_dsupers del_dsupers add_sels del_sels =
  run0 @@ fun (module C) ->
  let module U = Entity_utils (C) in
  let%lwt add_dsupers = Lwt_list.map_p U.Entity.select_one add_dsupers in
  let%lwt del_dsupers = Lwt_list.map_p U.Entity.select_one del_dsupers in
  let%lwt add_sels = Lwt_list.map_p U.lookup_add_selector add_sels in
  let%lwt del_sels = Lwt_list.map_p U.lookup_delete_selector del_sels in
  let%lwt e = U.Entity.select_one sel in
  Lwt_list.iter_s (fun e_sub -> C.Entity.force_dsub e e_sub) add_dsupers >>
  Lwt_list.iter_s (U.update_attributes e) add_sels >>
  Lwt_list.iter_s (U.update_attributes e) del_sels >>
  Lwt_list.iter_s (fun e_sub -> C.Entity.relax_dsub e e_sub) del_dsupers

let e_modify_t =
  let sel_t = Arg.(required & pos 0 (some selector_conv) None &
                   info ~docv:"PATH" []) in
  let add_succs_t = Arg.(value & opt_all selector_conv [] &
                         info ~docv:"PATH" ["s"]) in
  let del_succs_t = Arg.(value & opt_all selector_conv [] &
                         info ~docv:"PATH" ["r"]) in
  let add_attrs_t = Arg.(value & opt_all add_selector_conv [] &
                         info ~docv:"APATH" ["a"]) in
  let del_attrs_t = Arg.(value & opt_all delete_selector_conv [] &
                         info ~docv:"APATH" ["d"]) in
  Term.(pure e_modify $ sel_t $ add_succs_t $ del_succs_t
                      $ add_attrs_t $ del_attrs_t)
