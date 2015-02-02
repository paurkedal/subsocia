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

module type RPCM = Subsocia_rpc.RPCM with type 'a t = 'a Lwt.t

module Int32_map = Prime_enummap.Make (Int32)
module Int32_set = Prime_enumset.Make (Int32)
module Attribute_key_base = struct
  type 'a t1 = {ak_id : int32; ak_name : string; ak_type : 'a Type.t1}
  type t0 = Ex : 'a t1 -> t0
end

module Make (RPCM : RPCM) = struct
  module Raw = Subsocia_rpc.ClientM (RPCM)

  module Attribute_key = struct
    module Raw = Raw.Attribute_key

    include Attribute_key_base

    module Comparable = struct
      type t = t0
      let compare (Ex a) (Ex b) = compare a.ak_id b.ak_id
    end
    module Set = Prime_enumset.Make (Comparable)
    module Map = Prime_enummap.Make (Comparable)

    let of_name ak_name =
      lwt ak_id, Type.Ex ak_type = Raw.of_name ak_name in
      Lwt.return (Ex {ak_id; ak_name; ak_type})

    let of_id ak_id =
      lwt ak_name, Type.Ex ak_type = Raw.of_id ak_id in
      Lwt.return (Ex {ak_id; ak_name; ak_type})

    let id (Ex ak) = ak.ak_id
    let name (Ex ak) = Lwt.return ak.ak_name
    let type0 (Ex ak) = Type.Ex ak.ak_type
    let type1 ak = ak.ak_type
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

    let inclusion_preds et =
      Raw.inclusion_preds et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) *> Map.of_ordered_bindings

    let inclusion_succs et =
      Raw.inclusion_succs et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) *> Map.of_ordered_bindings

    let attribution lbt ubt =
      let aux (ak_id, mu) = Attribute_key.of_id ak_id >|= fun ak -> ak, mu in
      Raw.attribution lbt ubt >>= Lwt_list.map_s aux >|=
      Attribute_key.Map.of_ordered_bindings
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
    let minimums () = Raw.minimums () >|= Set.of_ordered_elements
    let maximums () = Raw.maximums () >|= Set.of_ordered_elements
    let preds e = Raw.preds e >|= Set.of_ordered_elements
    let succs e = Raw.succs e >|= Set.of_ordered_elements

    let fetch_attribute lb ub ak =
      Raw.fetch_attribute lb ub (Attribute_key.(id (Ex ak))) >|=
      List.map (Value.coerce (Attribute_key.type1 ak))

    let store_attribute lb ub ak vs =
      let t = Attribute_key.type1 ak in
      Raw.store_attribute lb ub (Attribute_key.(id (Ex ak)))
			  (List.map (fun v -> Value.Ex (t, v)) vs)

    let precedes = Raw.precedes
    let constrain = Raw.constrain
    let unconstrain = Raw.unconstrain
    let display_name ~langs e = Lwt.return ("#" ^ Int32.to_string e) (* TODO *)
  end
end