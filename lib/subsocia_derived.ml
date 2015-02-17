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

module Make (Config : CONFIG) (Sc : Subsocia_intf.S) = struct
  open Sc

  module Entity = struct

    let unit_entity () =
      match_lwt Entity.maximums () >|= Entity.Set.elements with
      | [ent] -> Lwt.return ent
      | [] ->
	Lwt.fail (Failure "The database is empty, \
			   there should be some initial data.")
      | _ ->
	Lwt.fail (Failure "Cannot find a unique unit entity as there \
			   are multiple maximal elements.")

    let display_name_ats_cache :
	  (lang, string Attribute_type.t1 list) Hashtbl.t = Hashtbl.create 5

    let coerce_to_string_at an at0 =
      let Attribute_type.Ex at1 = at0 in
      let coerce : type a. a Type.t1 * a Attribute_type.t1 ->
			   string Attribute_type.t1 =
	function
	| Type.String, at -> at
	| _ -> failwith (an ^ " is not a string attribute type.") in
      Lwt.wrap1 coerce (Attribute_type.type1 at1, at1)

    let display_name ~langs entity =
      lwt unit_entity = unit_entity () in

      let aux_plain an =
	match_lwt Attribute_type.of_name an with
	| None -> Lwt.return_none
	| Some at0 ->
	  lwt at = coerce_to_string_at an at0 in
	  lwt vs = Entity.getattr entity unit_entity at in
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
  end
end
