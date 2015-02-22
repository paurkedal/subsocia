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

open Panograph_i18n
open Printf
open Subsocia_common

module type CONFIG = sig
  val display_name_attributes : string list
end

let _fail fmt = ksprintf (fun s -> Lwt.fail (Failure s)) fmt

module Make (Config : CONFIG) (Base : Subsocia_intf.S) = struct
  open Base

  module Attribute_type = struct

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

    (* Predefined entity types. *)
    let et_unit = _et "unit"
    let et_access_group = _et "access_group"
    let et_auth_group = _et "auth_group"
    let et_person = _et "person"

    (* Predefined entities. *)
    let e_unit = Entity.of_id 1l

    let _e_un ?super en =
      lwt e_unit = match super with Some e -> Lwt.return e | None -> e_unit in
      lwt at_unique_name = at_unique_name in
      lwt es = Entity.apreds e_unit at_unique_name en in
      match Entity.Set.cardinal es with
      | 1 -> Lwt.return (Entity.Set.min_elt es)
      | 0 -> _fail "Missing initial entity %s" en
      | _ -> _fail "Multiple matches for unique name %s" en

    let e_forbidden = _e_un "Forbidden"
    let e_default_viewers = _e_un "Default Viewers"
    let e_default_admins = _e_un "Default Admins"
    let e_new_users = _e_un "New Users"
  end

  module Entity = struct

    let of_unique_name = Const._e_un

    let display_name_ats_cache :
	  (lang, string Base.Attribute_type.t1 list) Hashtbl.t =
      Hashtbl.create 5

    let display_name ~langs entity =
      lwt e_unit = Const.e_unit in

      let aux_plain an =
	match_lwt Base.Attribute_type.of_name an with
	| None -> Lwt.return_none
	| Some at0 ->
	  lwt at = Attribute_type.coerce_lwt Type.String at0 in
	  lwt vs = Entity.getattr entity e_unit at in
	  if Values.is_empty vs then Lwt.return_none
				else Lwt.return (Some (Values.min_elt vs)) in

      let aux_i18n an =
	Lwtx_list.search_s
	  (fun lang -> aux_plain (sprintf "%s[%s]" an (Lang.to_string lang)))
	  langs in

      let aux an =
	if Prime_string.has_suffix "[]" an
	then aux_i18n (Prime_string.slice 0 (String.length an - 2) an)
	else aux_plain an in

      match_lwt Lwtx_list.search_s aux Config.display_name_attributes with
      | Some s -> Lwt.return s
      | None -> Lwt.return @@ sprintf "# %ld" (Entity.id entity)

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
