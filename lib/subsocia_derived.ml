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

    let coerce (type a) (t : a Type.t) at0 : a Attribute_type.t option =
      let Attribute_type.Ex at1 = at0 in
      match t, Attribute_type.value_type at1, at1 with
      | Type.Bool, Type.Bool, at -> Some at
      | Type.Bool, _, _ -> None
      | Type.Int, Type.Int, at -> Some at
      | Type.Int, _, _ -> None
      | Type.String, Type.String, at -> Some at
      | Type.String, _, _ -> None

    let coerce_lwt (type a) (t : a Type.t) (Ex at1 as at0)
        : a Attribute_type.t Lwt.t =
      match coerce t at0 with
      | Some at -> Lwt.return at
      | None ->
        let%lwt an = Attribute_type.name at1 in
        let tn = Type.to_string (Attribute_type.value_type at1) in
        let tn' = Type.to_string t in
        _fail "Wrong type for %s : %s, expected %s." an tn tn'

    let required atn =
      match%lwt Base.Attribute_type.of_name atn with
      | Some at -> Lwt.return at
      | None -> _fail "Missing required attribute type %s" atn

    let typed_required vt atn =
      let%lwt at0 = required atn in
      match coerce vt at0 with
      | None -> _fail "Required attribute %s must have type %s"
                      atn (Type.to_string vt)
      | Some at -> Lwt.return at
  end

  module Attribute_uniqueness = Base.Attribute_uniqueness

  module Relation = struct
    include Base.Relation

    let (&&) q r =
      match q, r with
      | Inter qs, _ -> Inter (r :: qs)
      | _, Inter rs -> Inter (q :: rs)
      | _, _ -> Inter [q; r]
    let inter rs = Inter rs
    let present at = Present at
    let (=) at v = Eq (at, v)
    let (<:) at vs = In (at, vs)
    let (<::) at vs = at <: Values.of_elements (Attribute_type.value_type at) vs
    let (<=) at v = Leq (at, v)
    let (>=) at v = Geq (at, v)
    let between at v0 v1 = Between (at, v0, v1)
    let search at re = Search (at, re)
    let search_fts w = Search_fts w
  end

  module Entity_type = struct
    include Base.Entity_type

    let required etn =
      match%lwt Entity_type.of_name etn with
      | Some et -> Lwt.return et
      | None -> _fail "Missing required entity type %s" etn

    let equal et0 et1 = Base.Entity_type.compare et0 et1 = 0
  end

  module Const = struct

    let _et = Entity_type.required

    let _at_string = Attribute_type.typed_required Type.String

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

    let _e_un ?(from = Entity.root) en =
      let%lwt from = from in
      let%lwt at_unique_name = at_unique_name in
      let%lwt es = Entity.image1_eq at_unique_name en from in
      match Entity.Set.cardinal es with
      | 1 -> Lwt.return (Entity.Set.min_elt es)
      | 0 -> _fail "Missing initial entity %s" en
      | _ -> _fail "Multiple matches for unique name %s" en

    let e_default = _e_un "default"
    let e_new_users = _e_un ~from:e_default "subsocia-autoregs"
  end

  module Entity = struct
    include Base.Entity
    include Subsocia_selector.Selector_utils (Base)

    let equal e0 e1 = Base.Entity.compare e0 e1 = 0

    let force_sub subentity superentity =
      let%lwt is_sub = is_sub subentity superentity in
      if is_sub then Lwt.return_unit else force_dsub subentity superentity

    let getattr_opt e' e at =
      let%lwt vs = get_values at e e' in
      let n = Values.cardinal vs in
      if n = 0 then Lwt.return_none else
      if n = 1 then Lwt.return (Some (Values.min_elt vs)) else
      let%lwt an = Attribute_type.name at in
      _fail "Multiple matches for attribute %s" an

    let getattr_one e' e at =
      let%lwt vs = get_values at e' e in
      let n = Values.cardinal vs in
      if n = 1 then Lwt.return (Values.min_elt vs) else
      let%lwt an = Attribute_type.name at in
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
      let%lwt super =
        match super with Some e -> Lwt.return e | None -> Entity.root in
      let%lwt at_unique_name = Const.at_unique_name in
      let%lwt es = Entity.image1_eq at_unique_name en super in
      match Entity.Set.cardinal es with
      | 1 -> Lwt.return (Some (Entity.Set.min_elt es))
      | 0 -> Lwt.return_none
      | _ -> _fail "Multiple matches for unique name %s" en

    module Dsuper = struct
      let fold_s f e acc = dsuper e >>= (fun xs -> Set.fold_s f xs acc)
      let iter_s f e = dsuper e >>= Set.iter_s f
      let for_all_s f e = dsuper e >>= Set.for_all_s f
      let exists_s f e = dsuper e >>= Set.exists_s f
      let search_s f e = dsuper e >>= Set.search_s f
    end

    module Dsub = struct
      let fold_s f e acc = dsub e >>= (fun xs -> Set.fold_s f xs acc)
      let iter_s f e = dsub e >>= Set.iter_s f
      let for_all_s f e = dsub e >>= Set.for_all_s f
      let exists_s f e = dsub e >>= Set.exists_s f
      let search_s f e = dsub e >>= Set.search_s f
    end

    let make_visit () =
      let ht = Hashtbl.create 61 in
      fun e -> if Hashtbl.mem ht e then true else (Hashtbl.add ht e (); false)

    module Transitive (Dir : ITERABLE with type t := t) = struct
      exception Prune

      let rec fold_s' visit max_depth f e acc =
        if visit e then Lwt.return acc else
        try%lwt
          let%lwt acc = f e acc in
          if max_depth = 0 then Lwt.return acc else
          Dir.fold_s (fold_s' visit (max_depth - 1) f) e acc
        with Prune -> Lwt.return acc

      let fold_s ?(max_depth = max_int) f e acc =
        fold_s' (make_visit ()) max_depth f e acc

      let rec iter_s' visit max_depth f e =
        if visit e then Lwt.return_unit else
        try%lwt
          f e >>
          if max_depth = 0 then Lwt.return_unit else
          Dir.iter_s (iter_s' visit (max_depth - 1) f) e
        with Prune -> Lwt.return_unit

      let iter_s ?(max_depth = max_int) f e =
        iter_s' (make_visit ()) max_depth f e

      let rec for_all_s' visit max_depth f e =
        if visit e then Lwt.return_true else
        try%lwt
          match%lwt f e with
          | false -> Lwt.return_false
          | true ->
            if max_depth <= 0 then Lwt.return_true else
            Dir.for_all_s (for_all_s' visit (max_depth - 1) f) e
        with Prune -> Lwt.return_true

      let for_all_s ?(max_depth = max_int) f e =
        for_all_s' (make_visit ()) max_depth f e

      let rec exists_s' visit max_depth f e =
        if visit e then Lwt.return_false else
        try%lwt
          match%lwt f e with
          | true -> Lwt.return_true
          | false ->
            if max_depth <= 0 then Lwt.return_false else
            Dir.exists_s (exists_s' visit (max_depth - 1) f) e
        with Prune -> Lwt.return_false

      let exists_s ?(max_depth = max_int) f e =
        exists_s' (make_visit ()) max_depth f e

      let rec search_s' visit max_depth f e =
        if visit e then Lwt.return_none else
        try%lwt
          match%lwt f e with
          | Some _ as x -> Lwt.return x
          | None ->
            if max_depth <= 0 then Lwt.return_none else
            Dir.search_s (search_s' visit (max_depth - 1) f) e
        with Prune -> Lwt.return_none

      let search_s ?(max_depth = max_int) f e =
        search_s' (make_visit ()) max_depth f e
    end
    module Super = Transitive (Dsuper)
    module Sub = Transitive (Dsub)

    let has_role_for_entity role subj obj =
      let%lwt at_role = Const.at_role in
      let check_obj obj =
        let%lwt access_groups = image1_eq at_role role obj in
        Entity.Set.exists_s (Entity.is_sub subj) access_groups in
      match%lwt check_obj obj with
      | true -> Lwt.return_true
      | false -> Const.e_default >>= check_obj

    let can_view_entity = has_role_for_entity "subsocia.user"
    let can_edit_entity = has_role_for_entity "subsocia.admin"
    let can_search_below = has_role_for_entity "subsocia.user"

    let path_candidates = [
      ["unique_name"];
      ["role"];
      ["first_name"; "last_name"]
    ]

    let paths e =

      let%lwt r = Entity.rank e in
      let a = Array.make (r + 1) Entity.Map.empty in

      let inter _ selsA selsB =
        List.map (fun a -> List.map (fun b -> Select_inter (a, b)) selsB) selsA
          |> List.flatten |> Option.some in

      let add_conj e ps ats =
        let select_attr (type a) an (at : a Attribute_type.t) : a -> selector =
          match Attribute_type.value_type at with
          | Type.Bool -> fun v ->
            Select_image (Attribute_eq (an, string_of_bool v))
          | Type.Int -> fun v ->
            Select_image (Attribute_eq (an, string_of_int v))
          | Type.String -> fun v ->
            Select_image (Attribute_eq (an, v)) in
        let attr_by_succ (Attribute_type.Ex at) =
          let%lwt an = Attribute_type.name at in
          let attr vs = Values.elements vs |> List.map (select_attr an at) in
          Entity.premapping1 at e >|= Entity.Map.map attr in
        match%lwt Lwt_list.map_s attr_by_succ ats with
        | [] -> assert false
        | m :: ms ->
          Entity.Map.iter_s
            (fun e' sels ->
              let%lwt r' = rank e' in
              let ps' = List.flatten @@
                List.map (fun p -> List.map (fun sel -> sel :: p) sels) ps in
              let ps_acc = try Entity.Map.find e' a.(r') with Not_found -> [] in
              a.(r') <- Entity.Map.add e' (ps' @ ps_acc) a.(r');
              Lwt.return_unit)
            (List.fold (Entity.Map.finter inter) ms m) in

      let%lwt path_candidates =
        Lwt_list.map_s (Pwt_list.fmap_s Attribute_type.of_name)
                       path_candidates in
      a.(r) <- Entity.Map.singleton e [[]];
      for%lwt r' = r downto 1 do
        Entity.Map.iter_s
          (fun e ps -> Lwt_list.iter_s (add_conj e ps) path_candidates)
          a.(r')
      done >>

      let%lwt root = Entity.root in
      Lwt.return begin
        try
          let aux = function
            | [] -> Select_root
            | x :: xs -> List.fold (fun a y -> Select_with (y, a)) xs x in
          List.map aux (Entity.Map.find root a.(0))
        with Not_found -> []
      end

    let rec display_name_var ~context ~langs e spec =
      let%lwt root = Entity.root in

      let aux ?tn an =
        match%lwt Base.Attribute_type.of_name an with
        | None -> Lwt.return_none
        | Some at0 ->
          let%lwt at = Attribute_type.coerce_lwt Type.String at0 in
          match tn with
          | Some tn ->
            Entity.premapping1 at e >>=
            Base.Entity.Map.search_s
              (fun e' vs ->
                if Entity.Set.contains e' context then
                  Lwt.return (Some (Values.min_elt vs))
                else
                  match%lwt display_name_tmpl ~context ~langs e' with
                  | None ->
                    Lwt.return_none
                  | Some name ->
                    Lwt.return (Some (name ^ " / " ^ Values.min_elt vs)))
          | None ->
            let%lwt vs = Entity.get_values at root e in
            if Values.is_empty vs then Lwt.return_none
                                  else Lwt.return (Some (Values.min_elt vs)) in

      let tn, an =
        match Prime_string.cut_affix "/" spec with
        | None -> None, spec
        | Some (_ as tn, an) -> Some tn, an in
      match%lwt
        if Prime_string.has_suffix ".+" an then
          let an = Prime_string.slice 0 (String.length an - 2) an in
          Pwt_list.search_s
            (fun lang -> aux ?tn (sprintf "%s.%s" an (Lang.to_string lang)))
            langs
        else
          aux ?tn an
      with
      | None -> Lwt.fail Not_found
      | Some s -> Lwt.return s

    and display_name_tmpl ~context ~langs e =
      let aux tmpl =
        try%lwt
          let buf = Buffer.create 80 in
          let m = ref String_map.empty in
          Buffer.add_substitute buf (fun v -> m := String_map.add v () !m; "")
                                tmpl;
          Buffer.clear buf;
          let%lwt m = String_map.mapi_s
                        (fun v _ -> display_name_var ~context ~langs e v) !m in
          Buffer.add_substitute buf (fun v -> String_map.find v m) tmpl;
          Lwt.return (Some (Buffer.contents buf))
        with Not_found ->
          Lwt.return None in
      let%lwt et = Base.Entity.entity_type e in
      let%lwt tmpl = Base.Entity_type.entity_name_tmpl et in
      Pwt_list.search_s aux (Prime_string.chop_affix "|" tmpl)

    let display_name ?(context = Entity.Set.empty) ?(langs = []) e =
      display_name_tmpl ~context ~langs e >|=
      function
        | None -> sprintf "#%ld" (Entity.id e)
        | Some s -> s

    let candidate_dsupers ?(include_current = false) e =
      let%lwt et = Entity.entity_type e in
      let%lwt ets' = Entity_type.dsuper et in
      let not_related e' =
        match%lwt Entity.is_sub e e' with
        | true -> Lwt.return false
        | false -> Lwt.map not (Entity.is_sub e' e) in
      Entity_type.Map.fold
        (fun et' _ m_es ->
          let%lwt es = m_es in
          let%lwt s = Entity.type_members et' in
          let%lwt s =
            if include_current then Lwt.return s
                               else Entity.Set.filter_s not_related s in
          Lwt.return (Entity.Set.union s es))
        ets'
        (Lwt.return Entity.Set.empty)

  end
end
