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
open Printf
open Subsocia_common
open Unprime

let connect () =
  let uri = Uri.of_string Subsocia_config.database_uri#get in
  Subsocia_direct.connect uri

let run f = Lwt_main.run (f (connect ()))
let run0 f = Lwt_main.run (f (connect ())); 0

(* Entity Types *)

let et_create etn = run0 @@ fun (module C) ->
  lwt et = C.Entity_type.create etn in
  Lwt_log.info_f "Created type #%ld = %s." (C.Entity_type.id et) etn

let et_create_t =
  let et_name_t =
    Arg.(required & pos 0 (some string) None &
	 info [] ~docv:"TYPE-NAME" ~doc:"Name of type to create.") in
  Term.(pure et_create $ et_name_t)

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
    Arg.(required & pos 0 (some string) None & info ~docv:"TYPE-NAME" []) in
  Term.(ret (pure et_delete $ et_name_t))

let et_list () = run0 @@ fun (module C) ->
  C.Entity_type.all () >>=
  C.Entity_type.Set.iter_s
    (fun et -> C.Entity_type.name et >>= Lwt_io.printl)

let et_list_t = Term.(pure et_list $ pure ())

(* Inclusion Types *)

let it_allow etn0 etn1 = run @@ fun (module C) ->
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

let it_disallow etn0 etn1 = run @@ fun (module C) ->
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

let it_allow_t =
  let etn0_t = Arg.(required & pos 0 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(ret (pure it_allow $ etn0_t $ etn1_t))

let it_disallow_t =
  let etn0_t = Arg.(required & pos 0 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(required & pos 1 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(ret (pure it_disallow $ etn0_t $ etn1_t))

let it_list etn0_opt etn1_opt = run @@ fun (module C) ->
  let get_et = function
    | None | Some "_" -> Lwt.return_none
    | Some etn ->
      begin match_lwt C.Entity_type.of_name etn with
      | None -> Lwt.fail (Failure ("No entity type is named " ^ etn ^ "."))
      | Some et -> Lwt.return (Some et)
      end in
  lwt et0 = get_et etn0_opt in
  lwt et1 = get_et etn1_opt in
  let show_map = C.Entity_type.Map.iter_s @@ fun et (mu0, mu1) ->
    lwt etn = C.Entity_type.name et in
    Lwt_io.printlf "%s%s %s" (Multiplicity.to_string mu0)
			     (Multiplicity.to_string mu1) etn in
  match et0, et1 with
  | None, None ->
    Lwt.fail (Failure ("At least one entity type must be provided."))
  | Some et0, None ->
    C.Entity_type.inclusion_succs et0 >>= show_map >> Lwt.return 0
  | None, Some et1 ->
    C.Entity_type.inclusion_preds et1 >>= show_map >> Lwt.return 0
  | Some et0, Some et1 ->
    begin match_lwt C.Entity_type.inclusion et0 et1 with
    | Some (mu0, mu1) ->
      Lwt_io.printlf "%s%s" (Multiplicity.to_string mu0)
			    (Multiplicity.to_string mu1) >>
      Lwt.return 0
    | None ->
      Lwt.return 1
    end

let it_list_t =
  let etn0_t = Arg.(value & pos 0 (some string) None &
		    info ~docv:"SUB-TYPE" []) in
  let etn1_t = Arg.(value & pos 1 (some string) None &
		    info ~docv:"SUPER-TYPE" []) in
  Term.(pure it_list $ etn0_t $ etn1_t)

let subcommands = [
  et_list_t, Term.info ~doc:"List entity types." "et-list";
  et_create_t, Term.info ~doc:"Create an entity type." "et-create";
  et_delete_t, Term.info ~doc:"Delete an entity type." "et-delete";
  it_allow_t, Term.info ~doc:"Allow inclusion between entities of a type."
			"it-allow";
  it_disallow_t, Term.info ~doc:"Disallow inclusion between entities of a type."
			   "it-disallow";
  it_list_t, Term.info ~doc:"Show inclusion policy between types." "it-list";
]

let main_t = Term.(ret @@ pure (`Error (true, "Missing subcommand.")))

let () =
  match Term.eval_choice (main_t, Term.info "subsocia") subcommands with
  | `Error `Parse -> exit 64
  | `Error `Term -> exit 69
  | `Error `Exn -> exit 70
  | `Ok rc -> exit rc
  | `Version | `Help -> exit 0
