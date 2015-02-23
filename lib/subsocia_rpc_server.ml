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

open Subsocia_common
open Unprime
open Unprime_option

module Server_impl = struct

  type context = (module Subsocia_intf.S)

  type 'a t = 'a Lwt.t
  let return = Lwt.return
  let bind = Lwt.bind
  let fail = Lwt.fail
  let handle_failure = Lwt.catch

  module Attribute_type = struct

    let of_name (module C : Subsocia_intf.S) name =
      C.Attribute_type.of_name name >|= Option.map @@ fun ak ->
      C.Attribute_type.id ak, C.Attribute_type.type0 ak

    let of_id (module C : Subsocia_intf.S) id =
      lwt ak = C.Attribute_type.of_id id in
      lwt name = C.Attribute_type.name ak in
      Lwt.return (name, C.Attribute_type.type0 ak)

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

    let inclusion_preds (module C : Subsocia_intf.S) entity_id =
      C.Entity_type.of_id entity_id >>=
      C.Entity_type.inclusion_preds >|=
      C.Entity_type.Map.bindings *>
      List.map (fun (et, (muA, muB)) -> C.Entity_type.id et, muA, muB)

    let inclusion_succs (module C : Subsocia_intf.S) entity_id =
      C.Entity_type.of_id entity_id >>=
      C.Entity_type.inclusion_succs >|=
      C.Entity_type.Map.bindings *>
      List.map (fun (et, (muA, muB)) -> C.Entity_type.id et, muA, muB)

    let inclusion_allow (module C : Subsocia_intf.S) mu0 mu1 et0 et1 =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.inclusion_allow mu0 mu1 et0 et1

    let inclusion_disallow (module C : Subsocia_intf.S) et0 et1 =
      lwt et0 = C.Entity_type.of_id et0 in
      lwt et1 = C.Entity_type.of_id et1 in
      C.Entity_type.inclusion_disallow et0 et1

    let attribution_mult (module C : Subsocia_intf.S) lb_id ub_id ak_id =
      lwt lb = C.Entity_type.of_id lb_id in
      lwt ub = C.Entity_type.of_id ub_id in
      lwt ak = C.Attribute_type.of_id ak_id in
      C.Entity_type.attribution_mult0 lb ub ak

    let attribution (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity_type.of_id lb_id in
      lwt ub = C.Entity_type.of_id ub_id in
      C.Entity_type.attribution lb ub >|=
      C.Attribute_type.Map.bindings *>
      List.map (fun (ak, mu) -> C.Attribute_type.id ak, mu)
  end

  module Entity = struct
    let create (module C : Subsocia_intf.S) ~viewer ~admin entity_type =
      lwt viewer = C.Entity.of_id viewer in
      lwt admin = C.Entity.of_id admin in
      lwt entity_type = C.Entity_type.of_id entity_type in
      C.Entity.create ~viewer ~admin entity_type >|=
      C.Entity.id

    let type_ (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.type_ >|= C.Entity_type.id

    let viewer (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.viewer >|= C.Entity.id
    let admin (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.admin >|= C.Entity.id

    let type_members (module C : Subsocia_intf.S) et_id =
      lwt et = C.Entity_type.of_id et_id in
      C.Entity.type_members et >|= C.Entity.Set.elements *> List.map C.Entity.id
    let minimums (module C : Subsocia_intf.S) () =
      C.Entity.minimums () >|= C.Entity.Set.elements *> List.map C.Entity.id
    let maximums (module C : Subsocia_intf.S) () =
      C.Entity.maximums () >|= C.Entity.Set.elements *> List.map C.Entity.id
    let preds (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.preds >|=
      C.Entity.Set.elements *> List.map C.Entity.id
    let succs (module C : Subsocia_intf.S) e_id =
      C.Entity.of_id e_id >>= C.Entity.succs >|=
      C.Entity.Set.elements *> List.map C.Entity.id
    let precedes (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.precedes lb ub

    let getattr (module C : Subsocia_intf.S) lb_id ub_id ak_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      C.Entity.getattr lb ub ak >|=
      Values.elements *>
      List.map (fun v -> Value.Ex (C.Attribute_type.type1 ak, v))

    let setattr (module C : Subsocia_intf.S) lb_id ub_id ak_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 ak)) vs in
      C.Entity.setattr lb ub ak vs1

    let addattr (module C : Subsocia_intf.S) lb_id ub_id ak_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 ak)) vs in
      C.Entity.addattr lb ub ak vs1

    let delattr (module C : Subsocia_intf.S) lb_id ub_id ak_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 ak)) vs in
      C.Entity.delattr lb ub ak vs1

    let apreds (module C : Subsocia_intf.S) e_id ak_id v =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      let t = C.Attribute_type.type1 ak in
      C.Entity.apreds e ak (Value.coerce t v) >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let asuccs (module C : Subsocia_intf.S) e_id ak_id v =
      lwt e = C.Entity.of_id e_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      let t = C.Attribute_type.type1 ak in
      C.Entity.asuccs e ak (Value.coerce t v) >|=
      C.Entity.Set.elements *> List.map C.Entity.id

    let constrain (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.constrain lb ub

    let unconstrain (module C : Subsocia_intf.S) lb_id ub_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      C.Entity.unconstrain lb ub

  end
end

include Subsocia_rpc_primitives.ServerM (Server_impl)
