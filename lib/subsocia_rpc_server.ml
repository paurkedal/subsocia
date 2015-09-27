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

open Pwt_infix
open Subsocia_common
open Subsocia_rpc_types
open Unprime
open Unprime_list
open Unprime_option

module Utils (C : Subsocia_intf.S) = struct
  let decode_eap = function
    | Eap_present e_id ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      Lwt.return (C.Attribute.Present at)
    | Eap_eq (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x = Value.coerce (C.Attribute_type.value_type at) av in
      Lwt.return (C.Attribute.Eq (at, x))
    | Eap_in (e_id, avs) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let xs = Values.coerce (C.Attribute_type.value_type at) avs in
      Lwt.return (C.Attribute.In (at, xs))
    | Eap_leq (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x = Value.coerce (C.Attribute_type.value_type at) av in
      Lwt.return (C.Attribute.Leq (at, x))
    | Eap_geq (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x = Value.coerce (C.Attribute_type.value_type at) av in
      Lwt.return (C.Attribute.Geq (at, x))
    | Eap_between (e_id, av0, av1) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x0 = Value.coerce (C.Attribute_type.value_type at) av0 in
      let x1 = Value.coerce (C.Attribute_type.value_type at) av1 in
      Lwt.return (C.Attribute.Between (at, x0, x1))
    | Eap_search (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      begin match C.Attribute_type.value_type at with
      | Type.String -> Lwt.return (C.Attribute.Search (at, av))
      | _ -> assert false
      end
    | Eap_search_fts av ->
      Lwt.return (C.Attribute.Search_fts av)
end

module Server_impl = struct

  type context = (module Subsocia_intf.S)

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail
  let handle_failure = Lwt.catch

  module Attribute_type = struct

    let of_name (module C : Subsocia_intf.S) name =
      C.Attribute_type.of_name name >|=
      Option.map @@ fun (C.Attribute_type.Ex at) ->
	C.Attribute_type.id' at, Type.Ex (C.Attribute_type.value_type at),
	C.Attribute_type.value_mult at

    let of_id (module C : Subsocia_intf.S) id =
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id id in
      lwt name = C.Attribute_type.name at in
      Lwt.return (name, Type.Ex (C.Attribute_type.value_type at),
		  C.Attribute_type.value_mult at)

    let create (module C : Subsocia_intf.S) (Type.Ex vt) mult name =
      C.Attribute_type.create ~mult vt name >|= C.Attribute_type.id'

    let delete (module C : Subsocia_intf.S) id =
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id id in
      C.Attribute_type.delete at
  end

  module Attribute_uniqueness = struct

    let force (module C : Subsocia_intf.S) s =
      Lwt_list.map_s C.Attribute_type.of_id s >>=
      C.Attribute_type.Set.of_ordered_elements *>
      C.Attribute_uniqueness.force >|=
      C.Attribute_uniqueness.id

    let relax (module C : Subsocia_intf.S) u =
      C.Attribute_uniqueness.(of_id u >>= relax)

    let find (module C : Subsocia_intf.S) s =
      Lwt_list.map_s C.Attribute_type.of_id s >>=
      C.Attribute_type.Set.of_ordered_elements *>
      C.Attribute_uniqueness.find >|=
      Option.map C.Attribute_uniqueness.id

    let affecting (module C : Subsocia_intf.S) at_id =
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      C.Attribute_uniqueness.affecting at >|=
      C.Attribute_uniqueness.Set.elements *> List.map C.Attribute_uniqueness.id

    let affected (module C : Subsocia_intf.S) u =
      C.Attribute_uniqueness.of_id u >>=
      C.Attribute_uniqueness.affected >|=
      C.Attribute_type.Set.elements *>
      List.map (fun (C.Attribute_type.Ex at) -> C.Attribute_type.id' at)

  end

  module Entity_type = struct

    let of_name (module C : Subsocia_intf.S) name =
      C.Entity_type.of_name name >|= Option.map C.Entity_type.id

    let name (module C : Subsocia_intf.S) id =
      C.Entity_type.of_id id >>= C.Entity_type.name

    let create (module C : Subsocia_intf.S) name =
      C.Entity_type.create name >|= C.Entity_type.id

    let delete (module C : Subsocia_intf.S) id =
      C.Entity_type.of_id id >>= C.Entity_type.delete

    let all (module C : Subsocia_intf.S) () =
      C.Entity_type.all () >|=
      C.Entity_type.Set.elements *> List.map C.Entity_type.id

    let entity_name_tmpl (module C : Subsocia_intf.S) et =
      lwt et = C.Entity_type.of_id et in
      C.Entity_type.entity_name_tmpl et

    let set_entity_name_tmpl (module C : Subsocia_intf.S) et name =
      lwt et = C.Entity_type.of_id et in
      C.Entity_type.set_entity_name_tmpl et name

    let can_dsub (module C : Subsocia_intf.S) et0 et1 =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.can_dsub et0 et1

    let dsub (module C : Subsocia_intf.S) entity_id =
      C.Entity_type.of_id entity_id >>=
      C.Entity_type.dsub >|=
      C.Entity_type.Map.bindings *>
      List.map (fun (et, (muA, muB)) -> C.Entity_type.id et, muA, muB)

    let dsuper (module C : Subsocia_intf.S) entity_id =
      C.Entity_type.of_id entity_id >>=
      C.Entity_type.dsuper >|=
      C.Entity_type.Map.bindings *>
      List.map (fun (et, (muA, muB)) -> C.Entity_type.id et, muA, muB)

    let dsub_elements (module C : Subsocia_intf.S) () =
      C.Entity_type.dsub_elements () >|=
      List.map (fun (et0, et1, mu0, mu1) ->
		  C.Entity_type.(id et0, id et1, mu0, mu1))

    let allow_dsub (module C : Subsocia_intf.S) mu0 mu1 et0 et1 =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.allow_dsub mu0 mu1 et0 et1

    let disallow_dsub (module C : Subsocia_intf.S) et0 et1 =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.disallow_dsub et0 et1

    let can_attribute (module C : Subsocia_intf.S) at_id et0_id et1_id =
      lwt et0 = C.Entity_type.of_id et0_id in
      lwt et1 = C.Entity_type.of_id et1_id in
      lwt (C.Attribute_type.Ex at) = C.Attribute_type.of_id at_id in
      C.Entity_type.can_attribute at et0 et1

    let allowed_attributes (module C : Subsocia_intf.S) et0_id et1_id =
      lwt et0 = C.Entity_type.of_id et0_id in
      lwt et1 = C.Entity_type.of_id et1_id in
      C.Entity_type.allowed_attributes et0 et1 >|=
      C.Attribute_type.Set.elements *>
      List.map (fun (C.Attribute_type.Ex at) -> C.Attribute_type.id' at)

    let allowed_attributions (module C : Subsocia_intf.S) () =
      C.Entity_type.allowed_attributions () >|=
      List.map (fun (C.Attribute_type.Ex at, et0, et1) ->
		  (C.Attribute_type.id' at,
		   C.Entity_type.id et0, C.Entity_type.id et1))

    let allow_attribution (module C : Subsocia_intf.S) at et0 et1 =
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at in
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.allow_attribution at et0 et1

    let disallow_attribution (module C : Subsocia_intf.S) at et0 et1 =
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at in
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.disallow_attribution at et0 et1
  end

  module Entity = struct
    let create (module C : Subsocia_intf.S) ?access entity_type =
      lwt access = Pwt_option.map_s C.Entity.of_id access in
      lwt entity_type = C.Entity_type.of_id entity_type in
      C.Entity.create ?access entity_type >|=
      C.Entity.id

    let modify (module C : Subsocia_intf.S) ?access entity =
      lwt access = Pwt_option.map_s C.Entity.of_id access in
      lwt entity = C.Entity.of_id entity in
      C.Entity.modify ?access entity

    let delete (module C : Subsocia_intf.S) entity =
      lwt entity = C.Entity.of_id entity in
      C.Entity.delete entity

    let type_ (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.type_ >|= C.Entity_type.id

    let rank (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.rank

    let access (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.access >|= C.Entity.id

    let type_members (module C : Subsocia_intf.S) et_id =
      lwt et = C.Entity_type.of_id et_id in
      C.Entity.type_members et >|= C.Entity.Set.elements *> List.map C.Entity.id
    let top (module C : Subsocia_intf.S) () = C.Entity.top >|= C.Entity.id
    let minimums (module C : Subsocia_intf.S) () =
      C.Entity.minimums () >|= C.Entity.Set.elements *> List.map C.Entity.id
    let dsub (module C : Subsocia_intf.S) et_id e_id =
      lwt et = Pwt_option.map_s C.Entity_type.of_id et_id in
      lwt e = C.Entity.of_id e_id in
      C.Entity.dsub ?et e >|=
      C.Entity.Set.elements *> List.map C.Entity.id
    let dsuper (module C : Subsocia_intf.S) et_id e_id =
      lwt et = Pwt_option.map_s C.Entity_type.of_id et_id in
      lwt e = C.Entity.of_id e_id in
      C.Entity.dsuper ?et e >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let is_dsub (module C : Subsocia_intf.S) e_id e_id' =
      lwt e = C.Entity.of_id e_id in
      lwt e' = C.Entity.of_id e_id' in
      C.Entity.is_dsub e e'

    let is_sub (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.is_sub lb ub

    let get_values (module C : Subsocia_intf.S) at_id et0_id et1_id =
      lwt et0 = C.Entity.of_id et0_id in
      lwt et1 = C.Entity.of_id et1_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      C.Entity.get_values at et0 et1 >|=
      Values.elements *>
      List.map (fun v -> Value.Ex (C.Attribute_type.value_type at, v))

    let catch_uniqueness_error (module C : Subsocia_intf.S) f =
      try_lwt f () >> Lwt.return []
      with C.Attribute_uniqueness.Not_unique uas ->
	Lwt.return @@
	  List.map C.Attribute_uniqueness.id
		   (C.Attribute_uniqueness.Set.elements uas)

    let add_values (module C : Subsocia_intf.S) at_id vs et0_id et1_id =
      lwt et0 = C.Entity.of_id et0_id in
      lwt et1 = C.Entity.of_id et1_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let vt = C.Attribute_type.value_type at in
      let vs1 = Values.of_ordered_elements vt (List.map (Value.coerce vt) vs) in
      catch_uniqueness_error (module C)
			     (fun () -> C.Entity.add_values at vs1 et0 et1)

    let remove_values (module C : Subsocia_intf.S) at_id vs et0_id et1_id =
      lwt et0 = C.Entity.of_id et0_id in
      lwt et1 = C.Entity.of_id et1_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let vt = C.Attribute_type.value_type at in
      let vs1 = Values.of_ordered_elements vt (List.map (Value.coerce vt) vs) in
      C.Entity.remove_values at vs1 et0 et1

    let set_values (module C : Subsocia_intf.S) at_id vs et0_id et1_id =
      lwt et0 = C.Entity.of_id et0_id in
      lwt et1 = C.Entity.of_id et1_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let vt = C.Attribute_type.value_type at in
      let vs1 = Values.of_ordered_elements vt (List.map (Value.coerce vt) vs) in
      catch_uniqueness_error (module C)
			     (fun () -> C.Entity.set_values at vs1 et0 et1)

    let image1 (module C : Subsocia_intf.S) at_enc e_id =
      let module U = Utils (C) in
      lwt p = U.decode_eap at_enc in
      lwt e = C.Entity.of_id e_id in
      C.Entity.image1 p e >|= C.Entity.Set.elements *> List.map C.Entity.id

    let preimage1 (module C : Subsocia_intf.S) at_enc e_id =
      let module U = Utils (C) in
      lwt p = U.decode_eap at_enc in
      lwt e = C.Entity.of_id e_id in
      C.Entity.preimage1 p e >|= C.Entity.Set.elements *> List.map C.Entity.id

    let asub_conj (module C : Subsocia_intf.S) e_id ps_enc =
      let module U = Utils (C) in
      lwt e = C.Entity.of_id e_id in
      lwt ps = Lwt_list.map_s U.decode_eap ps_enc in
      C.Entity.asub_conj e ps
	>|= C.Entity.Set.elements *> List.map C.Entity.id

    let asuper_conj (module C : Subsocia_intf.S) e_id ps_enc =
      let module U = Utils (C) in
      lwt e = C.Entity.of_id e_id in
      lwt ps = Lwt_list.map_s U.decode_eap ps_enc in
      C.Entity.asuper_conj e ps
	>|= C.Entity.Set.elements *> List.map C.Entity.id

    let image1_eq (module C : Subsocia_intf.S) at_id v e_id =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let t = C.Attribute_type.value_type at in
      C.Entity.image1_eq at (Value.coerce t v) e >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let preimage1_eq (module C : Subsocia_intf.S) at_id v e_id =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let t = C.Attribute_type.value_type at in
      C.Entity.preimage1_eq at (Value.coerce t v) e >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let image1_fts (module C : Subsocia_intf.S)
		   ?entity_type ?super ?cutoff ?limit fts e_id =
      lwt e = C.Entity.of_id e_id in
      lwt entity_type = Pwt_option.map_s C.Entity_type.of_id entity_type in
      lwt super = Pwt_option.map_s C.Entity.of_id super in
      C.Entity.image1_fts ?entity_type ?super ?cutoff ?limit fts e >|=
      List.map (fun (e, w) -> (C.Entity.id e, w))

    let preimage1_fts (module C : Subsocia_intf.S)
		      ?entity_type ?super ?cutoff ?limit fts e_id =
      lwt e = C.Entity.of_id e_id in
      lwt entity_type = Pwt_option.map_s C.Entity_type.of_id entity_type in
      lwt super = Pwt_option.map_s C.Entity.of_id super in
      C.Entity.preimage1_fts ?entity_type ?super ?cutoff ?limit fts e >|=
      List.map (fun (e, w) -> (C.Entity.id e, w))

    let mapping1 (module C : Subsocia_intf.S) at_id e_id =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      lwt m = C.Entity.mapping1 at e in
      Lwt.return @@
	C.Entity.Map.fold
	  (fun e vs ->
	    Values.fold
	      (fun v ->
		let v0 = Value.Ex (C.Attribute_type.value_type at, v) in
		List.push (C.Entity.id e, v0))
	      vs)
	  m []

    let premapping1 (module C : Subsocia_intf.S) at_id e_id =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      lwt m = C.Entity.premapping1 at e in
      Lwt.return @@
	C.Entity.Map.fold
	  (fun e vs ->
	    Values.fold
	      (fun v ->
		let v0 = Value.Ex (C.Attribute_type.value_type at, v) in
		List.push (C.Entity.id e, v0))
	      vs)
	  m []

    let force_dsub (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.force_dsub lb ub

    let relax_dsub (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.relax_dsub lb ub

  end
end

include Subsocia_rpc_primitives.ServerM (Server_impl)
