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

open Iso639
open Lwt.Infix
open Lwt.Syntax
open Printf
open Subsocia_common
open Subsocia_derived_intf
open Subsocia_prereq
open Subsocia_selector_types
open Unprime_list

let _fail fmt =
  ksprintf (fun s -> Lwt.fail (Subsocia_error.Exn (`Msg s))) fmt

module Make (Base : Subsocia_intf.S) = struct
  open Base

  module Attribute_type = struct
    include Base.Attribute_type

    let coerce_any_lwt (type a) (t : a Type.t) (Any at1 as at0)
        : a Attribute_type.t Lwt.t =
      match coerce_any t at0 with
      | Some at -> Lwt.return at
      | None ->
        let* an = Attribute_type.name at1 in
        let tn = Type.to_string (Attribute_type.value_type at1) in
        let tn' = Type.to_string t in
        _fail "Wrong type for %s : %s, expected %s." an tn tn'
  end

  module Attribute_uniqueness = struct
    include Base.Attribute_uniqueness
    let soid_string au = soid au >|= Soid.to_string
  end

  module Relation = struct
    include Base.Relation

    let (&&) q r =
      match q, r with
      | Inter qs, _ -> Inter (r :: qs)
      | _, Inter rs -> Inter (q :: rs)
      | _, _ -> Inter [q; r]
    let inter rs = Inter rs
    let true_ = True
    let present at = Present at
    let (=) at v = Eq (at, v)
    let (<:) at vs = In (at, vs)
    let (<::) at vs = at <: Values.of_elements (Attribute_type.value_type at) vs
    let (<=) at v = Leq (at, v)
    let (>=) at v = Geq (at, v)
    let between at v0 v1 = Between (at, v0, v1)
    let search at re = Search (at, re)
    let search_fts w = Search_fts w

    let rec to_selector = function
     | Inter [] -> assert false
     | Inter (r :: rs) ->
        let aux r rs_sel =
          to_selector r >|= fun r_sel ->
          Select_inter (r_sel, rs_sel)
        in
        to_selector r >>= Lwt_list.fold_s aux rs
     | True ->
        Lwt.return (Select_image Neighbour)
     | Present at ->
        Attribute_type.name at >|= fun an -> Select_image (Attribute_present an)
     | Eq (at, av) ->
        Attribute_type.name at >|= fun an ->
        let v = Value.typed_to_string (Attribute_type.value_type at) av in
        Select_image (Attribute_eq (an, v))
     | In (at, avs) ->
        Attribute_type.name at >|= fun an ->
        (match
          avs |> Values.elements
              |> List.map (Value.typed_to_string (Attribute_type.value_type at))
         with
         | [] -> assert false
         | v :: vs ->
            let aux v sel =
              Select_union (Select_image (Attribute_eq (an, v)), sel)
            in
            List.fold aux vs (Select_image (Attribute_eq (an, v))))
     | Leq (at, av) ->
        Attribute_type.name at >|= fun an ->
        let v = Value.typed_to_string (Attribute_type.value_type at) av in
        Select_image (Attribute_leq (an, v))
     | Geq (at, av) ->
        Attribute_type.name at >|= fun an ->
        let v = Value.typed_to_string (Attribute_type.value_type at) av in
        Select_image (Attribute_geq (an, v))
     | Between (at, v_min, v_max) ->
        let* sel_min = to_selector (Geq (at, v_min)) in
        let* sel_max = to_selector (Leq (at, v_max)) in
        Lwt.return (Select_inter (sel_min, sel_max))
     | Search _ | Search_fts _ ->
        (* FIXME *)
        Subsocia_error.fail_lwt "Search to not supported for selectors."
  end

  module Entity_type = struct
    include Base.Entity_type

    let required etn =
      Entity_type.of_name etn >>= function
       | Some et -> Lwt.return et
       | None -> _fail "Missing required entity type %s" etn

    let equal et0 et1 = Base.Entity_type.compare et0 et1 = 0
  end

  module Const = struct

    let _et = Entity_type.required

    let _at_string = Attribute_type.of_name_exn Type.String

    (* Predefined attribute types. *)
    let at_unique_name = _at_string "unique_name"
    let at_proper_name = _at_string "proper_name"
    let at_first_name = _at_string "first_name"
    let at_last_name = _at_string "last_name"
    let at_email = _at_string "email"
    let at_role = _at_string "role"

    (* Predefined entity types. *)
    let et_root = _et "root"
    let et_access_group = _et "access_group"
    let et_auth_group = _et "auth_group"
    let et_person = _et "person"

    let _e_un ?from en =
      let* from = match from with
       | Some from -> from
       | None -> Entity.get_root ()
      in
      let* at_unique_name = at_unique_name in
      let* es = Entity.image1_eq at_unique_name en from in
      (match Entity.Set.cardinal es with
       | 1 -> Lwt.return (Entity.Set.min_elt_exn es)
       | 0 -> _fail "Missing initial entity %s" en
       | _ -> _fail "Multiple matches for unique name %s" en)

    let e_default = _e_un "default"
    let e_new_users = _e_un ~from:e_default "subsocia-autoregs"
  end

  module Entity = struct
    include Base.Entity
    include Subsocia_selector.Selector_utils (Base)

    let soid_string au = soid au >|= Soid.to_string

    let equal e0 e1 = Base.Entity.compare e0 e1 = 0

    let force_sub subentity superentity =
      let* is_sub = is_sub subentity superentity in
      if is_sub then Lwt.return_unit else force_dsub subentity superentity

    let getattr_opt e' e at =
      let* vs = get_values at e e' in
      let n = Values.cardinal vs in
      if n = 0 then Lwt.return_none else
      if n = 1 then Lwt.return (Some (Values.min_elt_exn vs)) else
      let* an = Attribute_type.name at in
      _fail "Multiple matches for attribute %s" an

    let getattr_one e' e at =
      let* vs = get_values at e' e in
      let n = Values.cardinal vs in
      if n = 1 then Lwt.return (Values.min_elt_exn vs) else
      let* an = Attribute_type.name at in
      if n = 0 then _fail "No matches for attribute %s" an
               else _fail "Multiple matches for attribute %s" an

    let add_value at v e e' =
      let vs = Values.singleton (Attribute_type.value_type at) v in
      add_values at vs e e'

    let remove_value at v e e' =
      let vs = Values.singleton (Attribute_type.value_type at) v in
      remove_values at vs e e'

    let set_value at v e e' =
      let vs = Values.singleton (Attribute_type.value_type at) v in
      set_values at vs e e'

    let clear_values at e e' =
      let vs = Values.empty (Attribute_type.value_type at) in
      set_values at vs e e'

    let of_unique_name ?super en =
      let* super = match super with
       | Some e -> Lwt.return e
       | None -> Entity.get_root ()
      in
      let* at_unique_name = Const.at_unique_name in
      let* es = Entity.image1_eq at_unique_name en super in
      (match Entity.Set.cardinal es with
       | 1 -> Lwt.return (Some (Entity.Set.min_elt_exn es))
       | 0 -> Lwt.return_none
       | _ -> _fail "Multiple matches for unique name %s" en)

    module Dsuper = struct
      let fold_s ?time f e acc =
        dsuper ?time e >>= (fun xs -> Set.fold_s f xs acc)
      let iter_s ?time f e = dsuper ?time e >>= Set.iter_s f
      let for_all_s ?time f e = dsuper ?time e >>= Set.for_all_s f
      let exists_s ?time f e = dsuper ?time e >>= Set.exists_s f
      let find_map_s ?time f e = dsuper ?time e >>= Set.find_map_s f
      let search_s = find_map_s
    end

    module Dsub = struct
      let fold_s ?time f e acc =
        dsub ?time e >>= (fun xs -> Set.fold_s f xs acc)
      let iter_s ?time f e = dsub ?time e >>= Set.iter_s f
      let for_all_s ?time f e = dsub ?time e >>= Set.for_all_s f
      let exists_s ?time f e = dsub ?time e >>= Set.exists_s f
      let find_map_s ?time f e = dsub ?time e >>= Set.find_map_s f
      let search_s = find_map_s
    end

    let make_visit () =
      let ht = Hashtbl.create 61 in
      fun e -> if Hashtbl.mem ht e then true else (Hashtbl.add ht e (); false)

    module Transitive (Dir : ITERABLE with type t := t) = struct
      exception Prune

      let rec fold_s' ?time visit max_depth f e acc =
        if visit e then Lwt.return acc else
        try%lwt
          let* acc = f e acc in
          if max_depth = 0 then Lwt.return acc else
          Dir.fold_s ?time (fold_s' ?time visit (max_depth - 1) f) e acc
        with Prune -> Lwt.return acc

      let fold_s ?time ?(max_depth = max_int) f e acc =
        fold_s' ?time (make_visit ()) max_depth f e acc

      let rec iter_s' ?time visit max_depth f e =
        if visit e then Lwt.return_unit else
        try%lwt
          f e >>= fun () ->
          if max_depth = 0 then Lwt.return_unit else
          Dir.iter_s ?time (iter_s' ?time visit (max_depth - 1) f) e
        with Prune -> Lwt.return_unit

      let iter_s ?time ?(max_depth = max_int) f e =
        iter_s' ?time (make_visit ()) max_depth f e

      let rec for_all_s' ?time visit max_depth f e =
        if visit e then Lwt.return_true else
        (match%lwt f e with
         | exception Prune -> Lwt.return_true
         | false -> Lwt.return_false
         | true ->
            if max_depth <= 0 then Lwt.return_true else
            Dir.for_all_s ?time (for_all_s' ?time visit (max_depth - 1) f) e)

      let for_all_s ?time ?(max_depth = max_int) f e =
        for_all_s' ?time (make_visit ()) max_depth f e

      let rec exists_s' ?time visit max_depth f e =
        if visit e then Lwt.return_false else
        (match%lwt f e with
         | exception Prune -> Lwt.return_false
         | true -> Lwt.return_true
         | false ->
            if max_depth <= 0 then Lwt.return_false else
            Dir.exists_s ?time (exists_s' ?time visit (max_depth - 1) f) e)

      let exists_s ?time ?(max_depth = max_int) f e =
        exists_s' ?time (make_visit ()) max_depth f e

      let rec find_map_s' ?time visit max_depth f e =
        if visit e then Lwt.return_none else
        (match%lwt f e with
         | exception Prune -> Lwt.return_none
         | Some _ as x -> Lwt.return x
         | None ->
            if max_depth <= 0 then Lwt.return_none else
            Dir.find_map_s ?time (find_map_s' ?time visit (max_depth - 1) f) e)

      let find_map_s ?time ?(max_depth = max_int) f e =
        find_map_s' ?time (make_visit ()) max_depth f e

      let search_s = find_map_s
    end
    module Super = Transitive (Dsuper)
    module Sub = Transitive (Dsub)

    let has_role_for_entity role subj obj =
      let* at_role = Const.at_role in
      let check_obj obj =
        let* access_groups = image1_eq at_role role obj in
        Entity.Set.exists_s (Entity.is_sub subj) access_groups
      in
      check_obj obj >>= function
       | true -> Lwt.return_true
       | false -> Const.e_default >>= check_obj

    let can_view_entity = has_role_for_entity "subsocia.user"
    let can_edit_entity = has_role_for_entity "subsocia.admin"
    let can_search_below = has_role_for_entity "subsocia.user"

    let build_attribute at vs = Relation.(Eq (at, Values.choose vs))

    let intersect_attributes ats e e' =
      let rec loop = function
       | [] -> fun acc -> Lwt.return_some Relation.(Inter acc)
       | Attribute_type.Any at :: ats -> fun acc ->
          let* vs = get_values at e e' in
          if Values.is_empty vs then Lwt.return_none else
          loop ats (build_attribute at vs :: acc)
      in
      loop ats

    let unique_mapping1 au e =
      Attribute_uniqueness.affected au >|= Attribute_type.Set.elements
      >>= function
       | [] -> assert false
       | Attribute_type.Any at :: ats ->
          mapping1 at e
            >|= Map.map (fun vs -> [build_attribute at vs])
            >>= Map.fmapi_s (intersect_attributes ats e)

    let unique_premapping1 au e =
      Attribute_uniqueness.affected au >|= Attribute_type.Set.elements
      >>= function
       | [] -> assert false
       | Attribute_type.Any at :: ats ->
          premapping1 at e
            >|= Map.map (fun vs -> [build_attribute at vs])
            >>= Map.fmapi_s (fun e' -> intersect_attributes ats e' e)

    let rec paths e =
      if%lwt Entity.is_root e then Lwt.return [Select_root] else
      let* et = Entity.entity_type e in
      let* apm = Entity_type.allowed_preimage et in
      let ats = apm
        |> Entity_type.Map.bindings
        |> List.rev_map snd |> List.rev_flatten
      in
      let* aus =
        let aux (Attribute_type.Any at) acc =
          Attribute_uniqueness.affecting at >|= fun aus ->
          Attribute_uniqueness.Set.union aus acc
        in
        Lwt_list.fold_s aux ats Attribute_uniqueness.Set.empty
      in
      let try_r (e', r) =
        let* ps' = paths e' in
        let* p = Relation.to_selector r in
        Lwt.return (List.map (fun p' -> Select_with (p', p)) ps')
      in
      let try_au au =
        unique_premapping1 au e >|= Entity.Map.bindings
          >>= Lwt_list.flatten_map_p try_r
      in
      Lwt_list.flatten_map_p try_au (Attribute_uniqueness.Set.elements aus)

    let rec display_name_var ~context ~langs e spec =
      let* root = Entity.get_root () in
      let try_source e' vs =
        if Entity.Set.mem e' context then
          Lwt.return (Some (Values.min_elt_exn vs))
        else
        display_name_tmpl ~context ~langs e' >>= function
         | None ->
            Lwt.return_none
         | Some name ->
            Lwt.return (Some (name ^ " / " ^ Values.min_elt_exn vs))
      in
      let try_attr ?tn an =
        (match%lwt Base.Attribute_type.any_of_name_exn an with
         | exception Subsocia_error.Exn (`Attribute_type_missing _) ->
            Lwt.return_none
         | at0 ->
            let* at = Attribute_type.coerce_any_lwt Type.String at0 in
            (match tn with
             | Some _tn ->
                Entity.premapping1 at e >>= Base.Entity.Map.find_map_s try_source
             | None ->
                let* vs = Entity.get_values at root e in
                if Values.is_empty vs then Lwt.return_none else
                Lwt.return (Some (Values.min_elt_exn vs))))
      in
      let try_lang ?tn an lang =
        try_attr ?tn (sprintf "%s.%s" an (Lang.to_string lang)) >>= function
         | Some s -> Lwt.return_some s
         | None ->
            (match Lang.to_iso639p1 lang with
             | Some lc -> try_attr ?tn (sprintf "%s.%s" an lc)
             | None -> Lwt.return_none)
      in
      let tn, an =
        (match Prime_string.cut_affix "/" spec with
         | None -> (None, spec)
         | Some (_ as tn, an) -> (Some tn, an))
      in
      begin
        if Prime_string.has_suffix ".+" an then
          let an = Prime_string.slice 0 (String.length an - 2) an in
          Lwt_list.find_map_s (try_lang ?tn an) langs
        else
          try_attr ?tn an
       end >>= function
        | None -> Lwt.fail Not_found
        | Some s -> Lwt.return s
    and display_name_tmpl ~context ~langs e =
      let aux tmpl =
        try%lwt
          let buf = Buffer.create 80 in
          let m = ref String_map.empty in
          Buffer.add_substitute buf
            (fun v -> m := String_map.add v () !m; "") tmpl;
          Buffer.clear buf;
          let* m =
            String_map.mapi_s
              (fun v _ -> display_name_var ~context ~langs e v) !m
          in
          Buffer.add_substitute buf (fun v -> String_map.find v m) tmpl;
          Lwt.return (Some (Buffer.contents buf))
        with Not_found ->
          Lwt.return None
      in
      let* et = Base.Entity.entity_type e in
      let* tmpl = Base.Entity_type.entity_name_tmpl et in
      Lwt_list.find_map_s aux (Prime_string.chop_affix "|" tmpl)

    let display_name ?(context = Entity.Set.empty) ?(langs = []) e =
      display_name_tmpl ~context ~langs e >>= function
       | None -> Entity.soid e >|= Soid.to_string
       | Some s -> Lwt.return s

    let candidate_dsupers ?(include_current = false) e =
      let* et = Entity.entity_type e in
      let* ets' = Entity_type.dsuper et in
      let not_related e' =
        Entity.is_sub e e' >>= function
         | true -> Lwt.return false
         | false -> Lwt.map not (Entity.is_sub e' e)
      in
      let aux et' _ m_es =
        let* es = m_es in
        let* s = Entity.type_members et' in
        let* s =
          if include_current then Lwt.return s else
          Entity.Set.filter_s not_related s
        in
        Lwt.return (Entity.Set.union s es)
      in
      Entity_type.Map.fold aux ets' (Lwt.return Entity.Set.empty)

  end
end
