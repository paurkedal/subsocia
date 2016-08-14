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

open Pwt_infix
open Subsocia_common
open Subsocia_rpc_types
open Unprime
open Unprime_list
open Unprime_option

module type CONTEXT = Subsocia_intf.S_SOID with type soid = int32

module Utils (C : CONTEXT) = struct
  let rec decode_eap = function
    | Eap_inter eaps ->
      Lwt_list.map_s decode_eap eaps >|= fun ps -> C.Relation.Inter ps
    | Eap_present e_id ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      Lwt.return (C.Relation.Present at)
    | Eap_eq (e_id, av) ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      let x = Value.coerce (C.Attribute_type.value_type at) av in
      Lwt.return (C.Relation.Eq (at, x))
    | Eap_in (e_id, avs) ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      let xs = Values.coerce (C.Attribute_type.value_type at) avs in
      Lwt.return (C.Relation.In (at, xs))
    | Eap_leq (e_id, av) ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      let x = Value.coerce (C.Attribute_type.value_type at) av in
      Lwt.return (C.Relation.Leq (at, x))
    | Eap_geq (e_id, av) ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      let x = Value.coerce (C.Attribute_type.value_type at) av in
      Lwt.return (C.Relation.Geq (at, x))
    | Eap_between (e_id, av0, av1) ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      let x0 = Value.coerce (C.Attribute_type.value_type at) av0 in
      let x1 = Value.coerce (C.Attribute_type.value_type at) av1 in
      Lwt.return (C.Relation.Between (at, x0, x1))
    | Eap_search (e_id, av) ->
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid e_id in
      begin match C.Attribute_type.value_type at with
      | Type.String -> Lwt.return (C.Relation.Search (at, av))
      | _ -> assert false
      end
    | Eap_search_fts av ->
      Lwt.return (C.Relation.Search_fts av)
end

module Server_impl = struct

  type context = (module CONTEXT)

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail
  let handle_failure = Lwt.catch

  module Attribute_type = struct

    let of_name (module C : CONTEXT) name =
      C.Attribute_type.of_name name >>=
      Pwt_option.map_s @@ fun (C.Attribute_type.Ex at) ->
        C.Attribute_type.soid at >|= fun at_id ->
        ( at_id,
          Type.Ex (C.Attribute_type.value_type at),
          C.Attribute_type.value_mult at )

    let of_soid (module C : CONTEXT) id =
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid id in
      let%lwt name = C.Attribute_type.name at in
      Lwt.return (name, Type.Ex (C.Attribute_type.value_type at),
                  C.Attribute_type.value_mult at)

    let create (module C : CONTEXT) (Type.Ex vt) mult name =
      C.Attribute_type.create ~mult vt name >>= C.Attribute_type.soid

    let delete (module C : CONTEXT) id =
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid id in
      C.Attribute_type.delete at

    let all (module C : CONTEXT) () =
      C.Attribute_type.all ()
        >|= C.Attribute_type.Set.elements
        >>= Lwt_list.map_s
              (fun (C.Attribute_type.Ex at) -> C.Attribute_type.soid at)
  end

  module Attribute_uniqueness = struct

    let force (module C : CONTEXT) s =
      Lwt_list.map_s C.Attribute_type.of_soid s
        >>= C.Attribute_type.Set.of_ordered_elements @>
            C.Attribute_uniqueness.force
        >>= C.Attribute_uniqueness.soid

    let relax (module C : CONTEXT) u =
      C.Attribute_uniqueness.(of_soid u >>= relax)

    let find (module C : CONTEXT) s =
      Lwt_list.map_s C.Attribute_type.of_soid s
        >>= C.Attribute_type.Set.of_ordered_elements @>
            C.Attribute_uniqueness.find
        >>= Pwt_option.map_s C.Attribute_uniqueness.soid

    let all (module C : CONTEXT) () =
      C.Attribute_uniqueness.all ()
        >|= C.Attribute_uniqueness.Set.elements
        >>= Lwt_list.map_s C.Attribute_uniqueness.soid

    let affecting (module C : CONTEXT) at_id =
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      C.Attribute_uniqueness.affecting at
        >|= C.Attribute_uniqueness.Set.elements
        >>= Lwt_list.map_s C.Attribute_uniqueness.soid

    let affected (module C : CONTEXT) u =
      C.Attribute_uniqueness.of_soid u
        >>= C.Attribute_uniqueness.affected
        >|= C.Attribute_type.Set.elements
        >>= Lwt_list.map_s
              (fun (C.Attribute_type.Ex at) -> C.Attribute_type.soid at)

  end

  module Entity_type = struct

    let of_name (module C : CONTEXT) name =
      C.Entity_type.of_name name >>= Pwt_option.map_s C.Entity_type.soid

    let name (module C : CONTEXT) id =
      C.Entity_type.of_soid id >>= C.Entity_type.name

    let create (module C : CONTEXT) name =
      C.Entity_type.create name >>= C.Entity_type.soid

    let delete (module C : CONTEXT) id =
      C.Entity_type.of_soid id >>= C.Entity_type.delete

    let all (module C : CONTEXT) () =
      C.Entity_type.all ()
        >|= C.Entity_type.Set.elements
        >>= Lwt_list.map_s C.Entity_type.soid

    let entity_name_tmpl (module C : CONTEXT) et =
      let%lwt et = C.Entity_type.of_soid et in
      C.Entity_type.entity_name_tmpl et

    let set_entity_name_tmpl (module C : CONTEXT) et name =
      let%lwt et = C.Entity_type.of_soid et in
      C.Entity_type.set_entity_name_tmpl et name

    let can_dsub (module C : CONTEXT) et0 et1 =
      let%lwt et0 = C.Entity_type.of_soid et0 in
      let%lwt et1 = C.Entity_type.of_soid et1 in
      C.Entity_type.can_dsub et0 et1


    let dsub (module C : CONTEXT) entity_id =
      C.Entity_type.of_soid entity_id
        >>= C.Entity_type.dsub
        >|= C.Entity_type.Map.bindings
        >>= Lwt_list.map_s
              (fun (et, (muA, muB)) ->
               C.Entity_type.soid et >|= fun et_id -> (et_id, muA, muB))

    let dsuper (module C : CONTEXT) entity_id =
      C.Entity_type.of_soid entity_id
        >>= C.Entity_type.dsuper
        >|= C.Entity_type.Map.bindings
        >>= Lwt_list.map_s
              (fun (et, (muA, muB)) ->
               C.Entity_type.soid et >|= fun et_id -> (et_id, muA, muB))

    let dsub_elements (module C : CONTEXT) () =
      C.Entity_type.dsub_elements ()
        >>= Lwt_list.map_s
              (fun (et0, et1, mu0, mu1) ->
                C.Entity_type.soid et0 >>= fun et0_id ->
                C.Entity_type.soid et1 >|= fun et1_id ->
                (et0_id, et1_id, mu0, mu1))

    let allow_dsub (module C : CONTEXT) mu0 mu1 et0 et1 =
      let%lwt et0 = C.Entity_type.of_soid et0 in
      let%lwt et1 = C.Entity_type.of_soid et1 in
      C.Entity_type.allow_dsub mu0 mu1 et0 et1

    let disallow_dsub (module C : CONTEXT) et0 et1 =
      let%lwt et0 = C.Entity_type.of_soid et0 in
      let%lwt et1 = C.Entity_type.of_soid et1 in
      C.Entity_type.disallow_dsub et0 et1

    let can_attribute (module C : CONTEXT) at_id et0_id et1_id =
      let%lwt et0 = C.Entity_type.of_soid et0_id in
      let%lwt et1 = C.Entity_type.of_soid et1_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      C.Entity_type.can_attribute at et0 et1

    let allowed_attributes (module C : CONTEXT) et0_id et1_id =
      let%lwt et0 = C.Entity_type.of_soid et0_id in
      let%lwt et1 = C.Entity_type.of_soid et1_id in
      C.Entity_type.allowed_attributes et0 et1
        >|= C.Attribute_type.Set.elements
        >>= Lwt_list.map_s
              (fun (C.Attribute_type.Ex at) -> C.Attribute_type.soid at)

    let allowed_preimage (module C : CONTEXT) et_id =
      let%lwt et = C.Entity_type.of_soid et_id in
      C.Entity_type.allowed_preimage et
        >>= Lwt_list.map_s
              (fun (C.Attribute_type.Ex at, et) ->
                C.Attribute_type.soid at >>= fun at_id ->
                C.Entity_type.soid et >|= fun et_id ->
                (at_id, et_id))

    let allowed_image (module C : CONTEXT) et_id =
      let%lwt et = C.Entity_type.of_soid et_id in
      C.Entity_type.allowed_image et
        >>= Lwt_list.map_s
              (fun (C.Attribute_type.Ex at, et) ->
                C.Attribute_type.soid at >>= fun at_id ->
                C.Entity_type.soid et >|= fun et_id ->
                (at_id, et_id))

    let allowed_mappings (module C : CONTEXT) at_id =
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      C.Entity_type.allowed_mappings at
        >>= Lwt_list.map_s
              (fun (et0, et1) ->
                C.Entity_type.soid et0 >>= fun et0_id ->
                C.Entity_type.soid et1 >|= fun et1_id ->
                (et0_id, et1_id))

    let allowed_attributions (module C : CONTEXT) () =
      C.Entity_type.allowed_attributions ()
        >>= Lwt_list.map_s
              (fun (C.Attribute_type.Ex at, et0, et1) ->
                C.Attribute_type.soid at >>= fun at_id ->
                C.Entity_type.soid et0 >>= fun et0_id ->
                C.Entity_type.soid et1 >|= fun et1_id ->
                (at_id, et0_id, et1_id))

    let allow_attribution (module C : CONTEXT) at et0 et1 =
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at in
      let%lwt et0 = C.Entity_type.of_soid et0 in
      let%lwt et1 = C.Entity_type.of_soid et1 in
      C.Entity_type.allow_attribution at et0 et1

    let disallow_attribution (module C : CONTEXT) at et0 et1 =
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at in
      let%lwt et0 = C.Entity_type.of_soid et0 in
      let%lwt et1 = C.Entity_type.of_soid et1 in
      C.Entity_type.disallow_attribution at et0 et1
  end

  module Entity = struct
    let create (module C : CONTEXT) et_id =
      let%lwt et = C.Entity_type.of_soid et_id in
      C.Entity.create et >>= C.Entity.soid

    let delete (module C : CONTEXT) e_id =
      let%lwt e = C.Entity.of_soid e_id in
      C.Entity.delete e

    let entity_type (module C : CONTEXT) e_id =
      C.Entity.of_soid e_id >>= C.Entity.entity_type >>= C.Entity_type.soid

    let rank (module C : CONTEXT) e_id =
      C.Entity.of_soid e_id >>= C.Entity.rank

    let type_members (module C : CONTEXT) et_id =
      let%lwt et = C.Entity_type.of_soid et_id in
      C.Entity.type_members et >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let is_root (module C : CONTEXT) e_id =
      C.Entity.of_soid e_id >>= C.Entity.is_root

    let root (module C : CONTEXT) () = C.Entity.root >>= C.Entity.soid

    let minimums (module C : CONTEXT) () =
      C.Entity.minimums () >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let dsub (module C : CONTEXT) et_id e_id =
      let%lwt et = Pwt_option.map_s C.Entity_type.of_soid et_id in
      let%lwt e = C.Entity.of_soid e_id in
      C.Entity.dsub ?et e >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let dsuper (module C : CONTEXT) et_id e_id =
      let%lwt et = Pwt_option.map_s C.Entity_type.of_soid et_id in
      let%lwt e = C.Entity.of_soid e_id in
      C.Entity.dsuper ?et e >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let is_dsub (module C : CONTEXT) e_id e_id' =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt e' = C.Entity.of_soid e_id' in
      C.Entity.is_dsub e e'

    let is_sub (module C : CONTEXT) lb_id ub_id =
      let%lwt lb = C.Entity.of_soid lb_id in
      let%lwt ub = C.Entity.of_soid ub_id in
      C.Entity.is_sub lb ub

    let get_values (module C : CONTEXT) at_id et0_id et1_id =
      let%lwt et0 = C.Entity.of_soid et0_id in
      let%lwt et1 = C.Entity.of_soid et1_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      C.Entity.get_values at et0 et1 >|=
      Values.elements @>
      List.map (fun v -> Value.Ex (C.Attribute_type.value_type at, v))

    let catch_uniqueness_error (module C : CONTEXT) f =
      try%lwt f () >> Lwt.return []
      with C.Attribute_uniqueness.Not_unique uas ->
        Lwt_list.map_s C.Attribute_uniqueness.soid
                       (C.Attribute_uniqueness.Set.elements uas)

    let add_values (module C : CONTEXT) at_id vs et0_id et1_id =
      let%lwt et0 = C.Entity.of_soid et0_id in
      let%lwt et1 = C.Entity.of_soid et1_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let vt = C.Attribute_type.value_type at in
      let vs1 = Values.of_ordered_elements vt (List.map (Value.coerce vt) vs) in
      catch_uniqueness_error (module C)
                             (fun () -> C.Entity.add_values at vs1 et0 et1)

    let remove_values (module C : CONTEXT) at_id vs et0_id et1_id =
      let%lwt et0 = C.Entity.of_soid et0_id in
      let%lwt et1 = C.Entity.of_soid et1_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let vt = C.Attribute_type.value_type at in
      let vs1 = Values.of_ordered_elements vt (List.map (Value.coerce vt) vs) in
      C.Entity.remove_values at vs1 et0 et1

    let set_values (module C : CONTEXT) at_id vs et0_id et1_id =
      let%lwt et0 = C.Entity.of_soid et0_id in
      let%lwt et1 = C.Entity.of_soid et1_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let vt = C.Attribute_type.value_type at in
      let vs1 = Values.of_ordered_elements vt (List.map (Value.coerce vt) vs) in
      catch_uniqueness_error (module C)
                             (fun () -> C.Entity.set_values at vs1 et0 et1)

    let image1 (module C : CONTEXT) at_enc e_id =
      let module U = Utils (C) in
      let%lwt p = U.decode_eap at_enc in
      let%lwt e = C.Entity.of_soid e_id in
      C.Entity.image1 p e
        >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let preimage1 (module C : CONTEXT) at_enc e_id =
      let module U = Utils (C) in
      let%lwt p = U.decode_eap at_enc in
      let%lwt e = C.Entity.of_soid e_id in
      C.Entity.preimage1 p e
        >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let image1_eq (module C : CONTEXT) at_id v e_id =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let t = C.Attribute_type.value_type at in
      C.Entity.image1_eq at (Value.coerce t v) e
        >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let preimage1_eq (module C : CONTEXT) at_id v e_id =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let t = C.Attribute_type.value_type at in
      C.Entity.preimage1_eq at (Value.coerce t v) e
        >|= C.Entity.Set.elements
        >>= Lwt_list.map_s C.Entity.soid

    let image1_fts (module C : CONTEXT)
                   ?entity_type ?super ?cutoff ?limit fts e_id =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt entity_type =
        Pwt_option.map_s C.Entity_type.of_soid entity_type in
      let%lwt super = Pwt_option.map_s C.Entity.of_soid super in
      C.Entity.image1_fts ?entity_type ?super ?cutoff ?limit fts e
        >>= Lwt_list.map_s
              (fun (e, w) -> C.Entity.soid e >|= fun e_id -> (e_id, w))

    let preimage1_fts (module C : CONTEXT)
                      ?entity_type ?super ?cutoff ?limit fts e_id =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt entity_type =
        Pwt_option.map_s C.Entity_type.of_soid entity_type in
      let%lwt super = Pwt_option.map_s C.Entity.of_soid super in
      C.Entity.preimage1_fts ?entity_type ?super ?cutoff ?limit fts e
        >>= Lwt_list.map_s
              (fun (e, w) -> C.Entity.soid e >|= fun e_id -> (e_id, w))

    let mapping1 (module C : CONTEXT) at_id e_id =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let%lwt m = C.Entity.mapping1 at e in
      C.Entity.Map.fold_s
        (fun e vs acc ->
          C.Entity.soid e >|= fun e_id ->
          Values.fold
            (fun v ->
              let v0 = Value.Ex (C.Attribute_type.value_type at, v) in
              List.push (e_id, v0))
            vs acc)
        m []

    let premapping1 (module C : CONTEXT) at_id e_id =
      let%lwt e = C.Entity.of_soid e_id in
      let%lwt C.Attribute_type.Ex at = C.Attribute_type.of_soid at_id in
      let%lwt m = C.Entity.premapping1 at e in
      C.Entity.Map.fold_s
        (fun e vs acc ->
          C.Entity.soid e >|= fun e_id ->
          Values.fold
            (fun v ->
              let v0 = Value.Ex (C.Attribute_type.value_type at, v) in
              List.push (e_id, v0))
            vs acc)
        m []

    let force_dsub (module C : CONTEXT) lb_id ub_id =
      let%lwt lb = C.Entity.of_soid lb_id in
      let%lwt ub = C.Entity.of_soid ub_id in
      C.Entity.force_dsub lb ub

    let relax_dsub (module C : CONTEXT) lb_id ub_id =
      let%lwt lb = C.Entity.of_soid lb_id in
      let%lwt ub = C.Entity.of_soid ub_id in
      C.Entity.relax_dsub lb ub

  end
end

include Subsocia_rpc_primitives.ServerM (Server_impl)
