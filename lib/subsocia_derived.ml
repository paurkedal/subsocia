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

open Panograph_i18n
open Printf
open Pwt_infix
open Subsocia_common
open Subsocia_derived_intf
open Subsocia_selector_types
open Unprime_list
open Unprime_option

let _fail fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt

module Make (Base : Subsocia_intf.S) = struct
  open Base

  module Attribute_type = struct
    include Base.Attribute_type

    let coerce (type a) (t : a Type.t1) at0 : a Attribute_type.t1 option =
      let Attribute_type.Ex at1 = at0 in
      match t, Attribute_type.type1 at1, at1 with
      | Type.Bool, Type.Bool, at -> Some at
      | Type.Bool, _, _ -> None
      | Type.Int, Type.Int, at -> Some at
      | Type.Int, _, _ -> None
      | Type.String, Type.String, at -> Some at
      | Type.String, _, _ -> None

    let coerce_lwt (type a) (t : a Type.t1) at0 : a Attribute_type.t1 Lwt.t =
      match coerce t at0 with
      | Some at -> Lwt.return at
      | None ->
	lwt an = Attribute_type.name at0 in
	let tn = Type.string_of_t0 (Attribute_type.type0 at0) in
	let tn' = Type.string_of_t1 t in
	_fail "Wrong type for %s : %s, expected %s." an tn tn'
  end

  module Entity_type = Base.Entity_type

  module Const = struct

    let _et etn =
      match_lwt Entity_type.of_name etn with
      | Some et -> Lwt.return et
      | None -> _fail "Missing required entity type %s" etn

    let _at atn =
      match_lwt Base.Attribute_type.of_name atn with
      | Some at -> Lwt.return at
      | None -> _fail "Missing required attribute type %s" atn

    let _at_string atn =
      lwt at0 = _at atn in
      match Attribute_type.coerce Type.String at0 with
      | None -> _fail "%s must be a string attribute type" atn
      | Some at -> Lwt.return at

    (* Predefined attribute types. *)
    let at_unique_name = _at_string "unique_name"
    let at_proper_name = _at_string "proper_name"
    let at_first_name = _at_string "first_name"
    let at_last_name = _at_string "last_name"
    let at_email = _at_string "email"
    let at_role = _at_string "role"

    (* Predefined entity types. *)
    let et_unit = _et "unit"
    let et_access_group = _et "access_group"
    let et_auth_group = _et "auth_group"
    let et_person = _et "person"

    let _e_un en =
      lwt top = Entity.top in
      lwt at_unique_name = at_unique_name in
      lwt es = Entity.apreds top at_unique_name en in
      match Entity.Set.cardinal es with
      | 1 -> Lwt.return (Entity.Set.min_elt es)
      | 0 -> _fail "Missing initial entity %s" en
      | _ -> _fail "Multiple matches for unique name %s" en

    let e_forbidden = _e_un "forbidden"
    let e_new_users = _e_un "registrations"
  end

  module Entity = struct
    include Base.Entity
    include Subsocia_selector.Selector_utils (Base)

    let getattr_opt e e' at =
      lwt vs = getattr e e' at in
      let n = Values.cardinal vs in
      if n = 0 then Lwt.return_none else
      if n = 1 then Lwt.return (Some (Values.min_elt vs)) else
      lwt an = Attribute_type.(name (Ex at)) in
      _fail "Multiple matches for attribute %s" an

    let getattr_one e e' at =
      lwt vs = getattr e e' at in
      let n = Values.cardinal vs in
      if n = 1 then Lwt.return (Values.min_elt vs) else
      lwt an = Attribute_type.(name (Ex at)) in
      if n = 0 then _fail "No matches for attribute %s" an
	       else _fail "Multiple matches for attribute %s" an

    let of_unique_name ?super en =
      lwt super = match super with Some e -> Lwt.return e
				 | None -> Entity.top in
      lwt at_unique_name = Const.at_unique_name in
      lwt es = Entity.apreds super at_unique_name en in
      match Entity.Set.cardinal es with
      | 1 -> Lwt.return (Some (Entity.Set.min_elt es))
      | 0 -> Lwt.return_none
      | _ -> _fail "Multiple matches for unique name %s" en

    module Dsucc = struct
      let fold_s f e acc = succs e >>= (fun xs -> Set.fold_s f xs acc)
      let iter_s f e = succs e >>= Set.iter_s f
      let for_all_s f e = succs e >>= Set.for_all_s f
      let exists_s f e = succs e >>= Set.exists_s f
      let search_s f e = succs e >>= Set.search_s f
    end

    module Dpred = struct
      let fold_s f e acc = preds e >>= (fun xs -> Set.fold_s f xs acc)
      let iter_s f e = preds e >>= Set.iter_s f
      let for_all_s f e = preds e >>= Set.for_all_s f
      let exists_s f e = preds e >>= Set.exists_s f
      let search_s f e = preds e >>= Set.search_s f
    end

    let make_visit () =
      let ht = Hashtbl.create 61 in
      fun e -> if Hashtbl.mem ht e then true else (Hashtbl.add ht e (); false)

    module Transitive (Dir : ITERABLE with type t := t) = struct
      exception Prune

      let rec fold_s' visit max_depth f e acc =
	if visit e then Lwt.return acc else
	try_lwt
	  lwt acc = f e acc in
	  if max_depth = 0 then Lwt.return acc else
	  Dir.fold_s (fold_s' visit (max_depth - 1) f) e acc
	with Prune -> Lwt.return acc

      let fold_s ?(max_depth = max_int) f e acc =
	fold_s' (make_visit ()) max_depth f e acc

      let rec iter_s' visit max_depth f e =
	if visit e then Lwt.return_unit else
	try_lwt
	  f e >>
	  if max_depth = 0 then Lwt.return_unit else
	  Dir.iter_s (iter_s' visit (max_depth - 1) f) e
	with Prune -> Lwt.return_unit

      let iter_s ?(max_depth = max_int) f e =
	iter_s' (make_visit ()) max_depth f e

      let rec for_all_s' visit max_depth f e =
	if visit e then Lwt.return_true else
	try_lwt
	  match_lwt f e with
	  | false -> Lwt.return_false
	  | true ->
	    if max_depth <= 0 then Lwt.return_true else
	    Dir.for_all_s (for_all_s' visit (max_depth - 1) f) e
	with Prune -> Lwt.return_true

      let for_all_s ?(max_depth = max_int) f e =
	for_all_s' (make_visit ()) max_depth f e

      let rec exists_s' visit max_depth f e =
	if visit e then Lwt.return_false else
	try_lwt
	  match_lwt f e with
	  | true -> Lwt.return_true
	  | false ->
	    if max_depth <= 0 then Lwt.return_false else
	    Dir.exists_s (exists_s' visit (max_depth - 1) f) e
	with Prune -> Lwt.return_false

      let exists_s ?(max_depth = max_int) f e =
	exists_s' (make_visit ()) max_depth f e

      let rec search_s' visit max_depth f e =
	if visit e then Lwt.return_none else
	try_lwt
	  match_lwt f e with
	  | Some _ as x -> Lwt.return x
	  | None ->
	    if max_depth <= 0 then Lwt.return_none else
	    Dir.search_s (search_s' visit (max_depth - 1) f) e
	with Prune -> Lwt.return_none

      let search_s ?(max_depth = max_int) f e =
	search_s' (make_visit ()) max_depth f e
    end
    module Tsucc = Transitive (Dsucc)
    module Tpred = Transitive (Dpred)

    let has_role role subj obj =
      lwt access_base = access obj in
      lwt at_role = Const.at_role in
      lwt access_groups = apreds access_base at_role role in
      Entity.Set.exists_s (Entity.precedes subj) access_groups

    let can_view = has_role "user"
    let can_edit = has_role "admin"

    let path_candidates = [
      ["unique_name"];
      ["role"];
      ["first_name"; "last_name"]
    ]

    let paths e =

      lwt r = Entity.rank e in
      let a = Array.make (r + 1) Entity.Map.empty in

      let inter _ selsA selsB =
	List.map (fun a -> List.map (fun b -> Select_inter (a, b)) selsB) selsA
	  |> List.flatten |> Option.some in

      let add_conj e ps ats =
	let select_attr (type a) an (at : a Attribute_type.t1) : a -> selector =
	  match Attribute_type.type1 at with
	  | Type.Bool -> fun v -> Select_attr (an, string_of_bool v)
	  | Type.Int -> fun v -> Select_attr (an, string_of_int v)
	  | Type.String -> fun v -> Select_attr (an, v) in
	let attr_by_succ (Attribute_type.Ex at as at0) =
	  lwt an = Attribute_type.name at0 in
	  let attr vs = Values.elements vs |> List.map (select_attr an at) in
	  Entity.atsuccs e at >|= Entity.Map.map attr in
	match_lwt Lwt_list.map_s attr_by_succ ats with
	| [] -> assert false
	| m :: ms ->
	  Entity.Map.iter_s
	    (fun e' sels ->
	      lwt r' = rank e' in
	      let ps' = List.flatten @@
		List.map (fun p -> List.map (fun sel -> sel :: p) sels) ps in
	      let ps_acc = try Entity.Map.find e' a.(r') with Not_found -> [] in
	      a.(r') <- Entity.Map.add e' (ps' @ ps_acc) a.(r');
	      Lwt.return_unit)
	    (List.fold (Entity.Map.finter inter) ms m) in

      lwt path_candidates =
	Lwt_list.map_s (Pwt_list.fmap_s Attribute_type.of_name)
		       path_candidates in
      a.(r) <- Entity.Map.singleton e [[]];
      for_lwt r' = r downto 1 do
	Entity.Map.iter_s
	  (fun e ps -> Lwt_list.iter_s (add_conj e ps) path_candidates)
	  a.(r')
      done >>

      lwt top = Entity.top in
      Lwt.return begin
	try
	  let aux = function
	    | [] -> Select_top
	    | x :: xs -> List.fold (fun a y -> Select_sub (y, a)) xs x in
	  List.map aux (Entity.Map.find top a.(0))
	with Not_found -> []
      end

    let resolve_attr ~langs e spec =
      lwt e_top = Entity.top in

      let aux_plain an =
	match_lwt Base.Attribute_type.of_name an with
	| None -> Lwt.return_none
	| Some at0 ->
	  lwt at = Attribute_type.coerce_lwt Type.String at0 in
	  lwt vs = Entity.getattr e e_top at in
	  if Values.is_empty vs then Lwt.return_none
				else Lwt.return (Some (Values.min_elt vs)) in

      let aux_i18n an =
	Pwt_list.search_s
	  (fun lang -> aux_plain (sprintf "%s:%s" an (Lang.to_string lang)))
	  langs in

      let aux an =
	if Prime_string.has_suffix ":+" an
	then aux_i18n (Prime_string.slice 0 (String.length an - 2) an)
	else aux_plain an in

      let comps = Prime_string.chop_affix "|" spec in
      match_lwt Pwt_list.search_s aux comps with
      | None -> raise_lwt Not_found
      | Some s -> Lwt.return s

    let display_name ~langs e =
      let aux tmpl =
	try_lwt
	  let buf = Buffer.create 80 in
	  let m = ref String_map.empty in
	  Buffer.add_substitute buf (fun v -> m := String_map.add v () !m; "")
				tmpl;
	  Buffer.clear buf;
	  lwt m = String_map.mapi_s (fun v _ -> resolve_attr ~langs e v) !m in
	  Buffer.add_substitute buf (fun v -> String_map.find v m) tmpl;
	  Lwt.return (Some (Buffer.contents buf))
	with Not_found -> Lwt.return None in
      lwt et = Base.Entity.type_ e in
      lwt tmpl = Base.Entity_type.entity_name_tmpl et in
      match_lwt Pwt_list.search_s aux (Prime_string.chop_affix "|" tmpl) with
      | Some s -> Lwt.return s
      | None -> Lwt.return @@ sprintf "# %ld" (Entity.id e)

    let candidate_succs e =
      lwt et = Entity.type_ e in
      lwt ets' = Entity_type.inclusion_succs et in
      let not_related e' =
	match_lwt Entity.precedes e e' with
	| true -> Lwt.return false
	| false -> Lwt.map not (Entity.precedes e' e) in
      Entity_type.Map.fold
	(fun et' _ m_es ->
	  lwt es = m_es in
	  lwt s = Entity.type_members et' in
	  lwt s = Entity.Set.filter_s not_related s in
	  Lwt.return (Entity.Set.union s es))
	ets'
	(Lwt.return Entity.Set.empty)
  end
end
