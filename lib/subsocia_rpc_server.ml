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
      let x = Value.coerce (C.Attribute_type.type1 at) av in
      Lwt.return (C.Attribute.Eq (at, x))
    | Eap_leq (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x = Value.coerce (C.Attribute_type.type1 at) av in
      Lwt.return (C.Attribute.Leq (at, x))
    | Eap_geq (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x = Value.coerce (C.Attribute_type.type1 at) av in
      Lwt.return (C.Attribute.Geq (at, x))
    | Eap_between (e_id, av0, av1) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      let x0 = Value.coerce (C.Attribute_type.type1 at) av0 in
      let x1 = Value.coerce (C.Attribute_type.type1 at) av1 in
      Lwt.return (C.Attribute.Between (at, x0, x1))
    | Eap_search (e_id, av) ->
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id e_id in
      begin match C.Attribute_type.type1 at with
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
      C.Attribute_type.of_name name >|= Option.map @@ fun at ->
      C.Attribute_type.id at, C.Attribute_type.type0 at

    let of_id (module C : Subsocia_intf.S) id =
      lwt at = C.Attribute_type.of_id id in
      lwt name = C.Attribute_type.name at in
      Lwt.return (name, C.Attribute_type.type0 at)

    let create (module C : Subsocia_intf.S) vt name =
      C.Attribute_type.create vt name >|= C.Attribute_type.id

    let delete (module C : Subsocia_intf.S) id =
      lwt at = C.Attribute_type.of_id id in
      C.Attribute_type.delete at
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

    let can_asub (module C : Subsocia_intf.S) lb_id ub_id at_id =
      lwt lb = C.Entity_type.of_id lb_id in
      lwt ub = C.Entity_type.of_id ub_id in
      lwt (C.Attribute_type.Ex at) = C.Attribute_type.of_id at_id in
      C.Entity_type.can_asub lb ub at

    let can_asub_byattr (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity_type.of_id lb_id in
      lwt ub = C.Entity_type.of_id ub_id in
      C.Entity_type.can_asub_byattr lb ub >|=
      C.Attribute_type.Map.bindings *>
      List.map (fun (at, mu) -> C.Attribute_type.id at, mu)

    let asub_elements (module C : Subsocia_intf.S) () =
      C.Entity_type.asub_elements () >|=
      List.map (fun (et0, et1, at, mu) ->
		  (C.Entity_type.id et0, C.Entity_type.id et1,
		   C.Attribute_type.id at, mu))

    let allow_asub (module C : Subsocia_intf.S) et0 et1 at mu =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      lwt at = C.Attribute_type.of_id at in
      C.Entity_type.allow_asub et0 et1 at mu

    let disallow_asub (module C : Subsocia_intf.S) et0 et1 at =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      lwt at = C.Attribute_type.of_id at in
      C.Entity_type.disallow_asub et0 et1 at
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
    let is_sub (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.is_sub lb ub

    let getattr (module C : Subsocia_intf.S) lb_id ub_id at_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      C.Entity.getattr lb ub at >|=
      Values.elements *>
      List.map (fun v -> Value.Ex (C.Attribute_type.type1 at, v))

    let setattr (module C : Subsocia_intf.S) lb_id ub_id at_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 at)) vs in
      C.Entity.setattr lb ub at vs1

    let addattr (module C : Subsocia_intf.S) lb_id ub_id at_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 at)) vs in
      C.Entity.addattr lb ub at vs1

    let delattr (module C : Subsocia_intf.S) lb_id ub_id at_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 at)) vs in
      C.Entity.delattr lb ub at vs1

    let asub (module C : Subsocia_intf.S) e_id at_enc =
      let module U = Utils (C) in
      lwt e = C.Entity.of_id e_id in
      lwt p = U.decode_eap at_enc in
      C.Entity.asub e p >|= C.Entity.Set.elements *> List.map C.Entity.id

    let asuper (module C : Subsocia_intf.S) e_id at_enc =
      let module U = Utils (C) in
      lwt e = C.Entity.of_id e_id in
      lwt p = U.decode_eap at_enc in
      C.Entity.asuper e p >|= C.Entity.Set.elements *> List.map C.Entity.id

    let asub_eq (module C : Subsocia_intf.S) e_id at_id v =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let t = C.Attribute_type.type1 at in
      C.Entity.asub_eq e at (Value.coerce t v) >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let asuper_eq (module C : Subsocia_intf.S) e_id at_id v =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      let t = C.Attribute_type.type1 at in
      C.Entity.asuper_eq e at (Value.coerce t v) >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let asub_fts (module C : Subsocia_intf.S) ~limit e_id fts =
      lwt e = C.Entity.of_id e_id in
      C.Entity.asub_fts ~limit e fts >|=
      List.map (fun (e, w) -> (C.Entity.id e, w))

    let asuper_fts (module C : Subsocia_intf.S) ~limit e_id fts =
      lwt e = C.Entity.of_id e_id in
      C.Entity.asuper_fts ~limit e fts >|=
      List.map (fun (e, w) -> (C.Entity.id e, w))

    let asub_get (module C : Subsocia_intf.S) e_id at_id =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      lwt m = C.Entity.asub_get e at in
      Lwt.return @@
	C.Entity.Map.fold
	  (fun e vs ->
	    Values.fold
	      (fun v ->
		let v0 = Value.Ex (C.Attribute_type.type1 at, v) in
		List.push (C.Entity.id e, v0))
	      vs)
	  m []

    let asuper_get (module C : Subsocia_intf.S) e_id at_id =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex at = C.Attribute_type.of_id at_id in
      lwt m = C.Entity.asuper_get e at in
      Lwt.return @@
	C.Entity.Map.fold
	  (fun e vs ->
	    Values.fold
	      (fun v ->
		let v0 = Value.Ex (C.Attribute_type.type1 at, v) in
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
