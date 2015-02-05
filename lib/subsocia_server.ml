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
      C.Attribute_type.of_name name >|= fun ak ->
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

    let fetch_attribute (module C : Subsocia_intf.S) lb_id ub_id ak_id =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      C.Entity.fetch_attribute lb ub ak >|=
      List.map (fun v -> Value.Ex (C.Attribute_type.type1 ak, v))

    let store_attribute (module C : Subsocia_intf.S) lb_id ub_id ak_id vs =
      lwt lb = C.Entity.of_id lb_id in
      lwt ub = C.Entity.of_id ub_id in
      lwt C.Attribute_type.Ex ak = C.Attribute_type.of_id ak_id in
      let vs1 = List.map (Value.coerce (C.Attribute_type.type1 ak)) vs in
      C.Entity.store_attribute lb ub ak vs1

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

include Subsocia_rpc.ServerM (Server_impl)
