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

module type RPCM = Subsocia_rpc_primitives.RPCM with type 'a t = 'a Lwt.t

module Attribute_type_base = struct
  type 'a t1 = {ak_id : int32; ak_name : string; ak_type : 'a Type.t1}
  type t0 = Ex : 'a t1 -> t0
end

module Make (RPCM : RPCM) = struct
  module Raw = Subsocia_rpc_primitives.ClientM (RPCM)

  module Attribute_type = struct
    module Raw = Raw.Attribute_type

    include Attribute_type_base

    module Comparable = struct
      type t = t0
      let compare (Ex a) (Ex b) = compare a.ak_id b.ak_id
    end
    module Set = Prime_enumset.Make_monadic (Comparable) (Lwt)
    module Map = Prime_enummap.Make_monadic (Comparable) (Lwt)

    let of_name ak_name =
      Raw.of_name ak_name >|=
      Option.map @@ fun (ak_id, Type.Ex ak_type) -> Ex {ak_id; ak_name; ak_type}

    let of_id ak_id =
      lwt ak_name, Type.Ex ak_type = Raw.of_id ak_id in
      Lwt.return (Ex {ak_id; ak_name; ak_type})

    let id (Ex ak) = ak.ak_id
    let name (Ex ak) = Lwt.return ak.ak_name
    let type0 (Ex ak) = Type.Ex ak.ak_type
    let type1 ak = ak.ak_type
    let create vt ak_name =
      Raw.create vt ak_name >|= fun ak_id ->
      let Type.Ex ak_type = vt in
      Ex {ak_id; ak_name; ak_type}
    let delete (Ex ak) = Raw.delete ak.ak_id
  end

  module Entity_type = struct
    module Raw = Raw.Entity_type

    module Set = Int32_set
    module Map = Int32_map

    type t = int32

    let of_name = Raw.of_name

    let id et = et
    let of_id et = Lwt.return et
    let compare = compare

    let name = Raw.name
    let display_name ~langs ?pl et = name et (* TODO *)

    let create = Raw.create
    let delete = Raw.delete
    let all () = Raw.all () >|= Set.of_ordered_elements

    let inclusion et0 et1 =
      Raw.inclusion et0 et1

    let inclusion_preds et =
      Raw.inclusion_preds et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) *> Map.of_ordered_bindings

    let inclusion_succs et =
      Raw.inclusion_succs et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) *> Map.of_ordered_bindings

    let inclusion_dump () =
      Raw.inclusion_dump ()

    let inclusion_allow mu0 mu1 et0 et1 =
      Raw.inclusion_allow mu0 mu1 et0 et1

    let inclusion_disallow et0 et1 =
      Raw.inclusion_disallow et0 et1

    let attribution_mult0 lbt ubt ak =
      Raw.attribution_mult lbt ubt (Attribute_type.id ak)

    let attribution_mult1 lbt ubt ak =
      Raw.attribution_mult lbt ubt (Attribute_type.id (Attribute_type.Ex ak))

    let attribution lbt ubt =
      let aux (ak_id, mu) = Attribute_type.of_id ak_id >|= fun ak -> ak, mu in
      Raw.attribution lbt ubt >>= Lwt_list.map_s aux >|=
      Attribute_type.Map.of_ordered_bindings

    let attribution_dump () =
      let aux (et0, et1, ak_id, mu) =
	Attribute_type.of_id ak_id >|= fun ak -> (et0, et1, ak, mu) in
      Raw.attribution_dump () >>= Lwt_list.map_s aux

    let attribution_allow et0 et1 (Attribute_type.Ex ak) mu =
      Raw.attribution_allow et0 et1 ak.Attribute_type.ak_id mu

    let attribution_disallow et0 et1 (Attribute_type.Ex ak) =
      Raw.attribution_disallow et0 et1 ak.Attribute_type.ak_id
  end

  module Entity = struct
    module Raw = Raw.Entity

    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    let id e = e
    let of_id e = Lwt.return e
    let compare = compare

    let create = Raw.create
    let type_ = Raw.type_
    let viewer = Raw.viewer
    let admin = Raw.admin
    let type_members et = Raw.type_members et >|= Set.of_ordered_elements
    let minimums () = Raw.minimums () >|= Set.of_ordered_elements
    let maximums () = Raw.maximums () >|= Set.of_ordered_elements
    let preds e = Raw.preds e >|= Set.of_ordered_elements
    let succs e = Raw.succs e >|= Set.of_ordered_elements

    let getattr lb ub ak =
      let t1 = Attribute_type.type1 ak in
      Raw.getattr lb ub (Attribute_type.(id (Ex ak))) >|=
      List.map (Value.coerce t1) *> Values.of_ordered_elements t1

    let setattr lb ub ak vs =
      let t = Attribute_type.type1 ak in
      Raw.setattr lb ub (Attribute_type.(id (Ex ak)))
		  (List.map (fun v -> Value.Ex (t, v)) vs)

    let addattr lb ub ak vs =
      let t = Attribute_type.type1 ak in
      Raw.addattr lb ub (Attribute_type.(id (Ex ak)))
		  (List.map (fun v -> Value.Ex (t, v)) vs)

    let delattr lb ub ak vs =
      let t = Attribute_type.type1 ak in
      Raw.delattr lb ub (Attribute_type.(id (Ex ak)))
		  (List.map (fun v -> Value.Ex (t, v)) vs)

    let apreds e ak av =
      let t = Attribute_type.type1 ak in
      Raw.apreds e (Attribute_type.(id (Ex ak))) (Value.Ex (t, av))
	>|= Set.of_ordered_elements

    let asuccs e ak av =
      let t = Attribute_type.type1 ak in
      Raw.asuccs e (Attribute_type.(id (Ex ak))) (Value.Ex (t, av))
	>|= Set.of_ordered_elements

    let precedes = Raw.precedes
    let constrain = Raw.constrain
    let unconstrain = Raw.unconstrain
    let display_name ~langs e = Lwt.return ("#" ^ Int32.to_string e) (* TODO *)
  end
end
