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
open Subsocia_prereq
open Subsocia_rpc_types
open Unprime
open Unprime_list
open Unprime_option

module type RPCM = Subsocia_rpc_primitives.RPCM with type 'a t = 'a Lwt.t

module Attribute_type_base = struct
  type 'a t = {at_id : int32; at_name : string; at_type : 'a Type.t}
  type ex = Ex : 'a t -> ex

  (**/**)
  type 'a t1 = 'a t
end

module Make (RPCM : RPCM) = struct
  module Raw = Subsocia_rpc_primitives.ClientM (RPCM)

  module Attribute_type = struct
    module Raw = Raw.Attribute_type

    include Attribute_type_base

    module Comparable = struct
      type t = ex
      let compare (Ex a) (Ex b) = compare a.at_id b.at_id
    end
    module Set = Prime_enumset.Make_monadic (Comparable) (Lwt)
    module Map = Prime_enummap.Make_monadic (Comparable) (Lwt)

    let of_id at_id =
      lwt at_name, Type.Ex at_type = Raw.of_id at_id in
      Lwt.return (Ex {at_id; at_name; at_type})
    let id (Ex at) = at.at_id
    let id' at = at.at_id

    let decode_set s = List.map (fun (Ex {at_id}) -> at_id) (Set.elements s)
    let encode_set ids = Lwt_list.map_s of_id ids >|= Set.of_ordered_elements

    let of_name at_name =
      Raw.of_name at_name >|=
      Option.map @@ fun (at_id, Type.Ex at_type) -> Ex {at_id; at_name; at_type}
    let name (Ex at) = Lwt.return at.at_name
    let name' at = Lwt.return at.at_name
    let value_type at = at.at_type
    let create' vt at_name =
      Raw.create (Type.Ex vt) at_name >|= fun at_id ->
      {at_id; at_name; at_type = vt}
    let delete' at = Raw.delete at.at_id

    (**/**)
    type t0 = ex
    let type0 (Ex at) = Type.Ex at.at_type
    let type1 = value_type
    let create (Type.Ex vt) name = create' vt name >|= fun at -> Ex at
    let delete (Ex at) = delete' at
  end

  module Attribute_uniqueness = struct
    module Raw = Raw.Attribute_uniqueness
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    let of_id = Lwt.return
    let id u = u

    let encode_set ids = Lwt_list.map_s of_id ids >|= Set.of_ordered_elements

    let force s = Raw.force (Attribute_type.decode_set s)
    let relax u = Raw.relax u
    let find s = Raw.find (Attribute_type.decode_set s)
    let affecting at = Raw.affecting (Attribute_type.id' at) >>= encode_set
    let affected u = Raw.affected u >>= Attribute_type.encode_set
  end

  module Attribute = struct
    type ex = Ex : 'a Attribute_type.t * 'a -> ex
    type predicate =
      | Present : 'a Attribute_type.t -> predicate
      | Eq : 'a Attribute_type.t * 'a -> predicate
      | In : 'a Attribute_type.t * 'a Values.t -> predicate
      | Leq : 'a Attribute_type.t * 'a -> predicate
      | Geq : 'a Attribute_type.t * 'a -> predicate
      | Between : 'a Attribute_type.t * 'a * 'a -> predicate
      | Search : string Attribute_type.t * string -> predicate
      | Search_fts : string -> predicate

    (**/**)
    type t0 = ex
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
    let entity_name_tmpl = Raw.entity_name_tmpl
    let set_entity_name_tmpl = Raw.set_entity_name_tmpl

    let can_dsub et0 et1 =
      Raw.can_dsub et0 et1

    let dsub et =
      Raw.dsub et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) *> Map.of_ordered_bindings

    let dsuper et =
      Raw.dsuper et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) *> Map.of_ordered_bindings

    let dsub_elements () =
      Raw.dsub_elements ()

    let allow_dsub mu0 mu1 et0 et1 =
      Raw.allow_dsub mu0 mu1 et0 et1

    let disallow_dsub et0 et1 =
      Raw.disallow_dsub et0 et1

    let can_asub lbt ubt at =
      Raw.can_asub lbt ubt (Attribute_type.id (Attribute_type.Ex at))

    let can_asub_byattr lbt ubt =
      let aux (at_id, mu) = Attribute_type.of_id at_id >|= fun at -> at, mu in
      Raw.can_asub_byattr lbt ubt >>= Lwt_list.map_s aux >|=
      Attribute_type.Map.of_ordered_bindings

    let asub_elements () =
      let aux (et0, et1, at_id, mu) =
	Attribute_type.of_id at_id >|= fun at -> (et0, et1, at, mu) in
      Raw.asub_elements () >>= Lwt_list.map_s aux

    let allow_asub et0 et1 (Attribute_type.Ex at) mu =
      Raw.allow_asub et0 et1 at.Attribute_type.at_id mu

    let disallow_asub et0 et1 (Attribute_type.Ex at) =
      Raw.disallow_asub et0 et1 at.Attribute_type.at_id
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
    let modify = Raw.modify
    let delete = Raw.delete
    let type_ = Raw.type_
    let rank = Raw.rank
    let access = Raw.access
    let type_members et = Raw.type_members et >|= Set.of_ordered_elements
    let top = Raw.top ()
    let minimums () = Raw.minimums () >|= Set.of_ordered_elements
    let dsub ?et e = Raw.dsub et e >|= Set.of_ordered_elements
    let dsuper ?et e = Raw.dsuper et e >|= Set.of_ordered_elements

    let getattr lb ub at =
      let t1 = Attribute_type.value_type at in
      Raw.getattr lb ub (Attribute_type.(id (Ex at))) >|=
      List.map (Value.coerce t1) *> Values.of_ordered_elements t1

    let setattr lb ub at vs =
      let t = Attribute_type.value_type at in
      Raw.setattr lb ub (Attribute_type.(id (Ex at)))
		  (List.map (fun v -> Value.Ex (t, v)) vs)

    let addattr lb ub at vs =
      let t = Attribute_type.value_type at in
      Raw.addattr lb ub (Attribute_type.(id (Ex at)))
		  (List.map (fun v -> Value.Ex (t, v)) vs)

    let delattr lb ub at vs =
      let t = Attribute_type.value_type at in
      Raw.delattr lb ub (Attribute_type.(id (Ex at)))
		  (List.map (fun v -> Value.Ex (t, v)) vs)

    let encode_predicate = function
      | Attribute.Present at -> Eap_present (Attribute_type.(id (Ex at)))
      | Attribute.Eq (at, x) ->
	let t = Attribute_type.value_type at in
	Eap_eq (Attribute_type.(id' at), Value.Ex (t, x))
      | Attribute.In (at, vs) ->
	let t = Attribute_type.value_type at in
	Eap_in (Attribute_type.(id' at), Values.Ex (t, vs))
      | Attribute.Leq (at, x) ->
	let t = Attribute_type.value_type at in
	Eap_leq (Attribute_type.(id' at), Value.Ex (t, x))
      | Attribute.Geq (at, x) ->
	let t = Attribute_type.value_type at in
	Eap_geq (Attribute_type.(id' at), Value.Ex (t, x))
      | Attribute.Between (at, x0, x1) ->
	let t = Attribute_type.value_type at in
	Eap_between (Attribute_type.(id (Ex at)),
		     Value.Ex (t, x0), Value.Ex (t, x1))
      | Attribute.Search (at, x) ->
	Eap_search (Attribute_type.(id (Ex at)), x)
      | Attribute.Search_fts x ->
	Eap_search_fts x

    let asub e p =
      Raw.asub e (encode_predicate p) >|= Set.of_ordered_elements
    let asuper e p =
      Raw.asuper e (encode_predicate p) >|= Set.of_ordered_elements

    let asub_conj e ps =
      Raw.asub_conj e (List.map encode_predicate ps)
	>|= Set.of_ordered_elements
    let asuper_conj e ps =
      Raw.asuper_conj e (List.map encode_predicate ps)
	>|= Set.of_ordered_elements

    let asub_eq e at av =
      let t = Attribute_type.value_type at in
      Raw.asub_eq e (Attribute_type.(id (Ex at))) (Value.Ex (t, av))
	>|= Set.of_ordered_elements

    let asuper_eq e at av =
      let t = Attribute_type.value_type at in
      Raw.asuper_eq e (Attribute_type.(id (Ex at))) (Value.Ex (t, av))
	>|= Set.of_ordered_elements

    let asub_fts = Raw.asub_fts
    let asuper_fts = Raw.asuper_fts

    let asub_get e at =
      let t = Attribute_type.value_type at in
      Raw.asub_get e (Attribute_type.(id (Ex at))) >|= fun bindings ->
      List.fold
	(fun (e, v) m ->
	  let vs = try Map.find e m with Not_found -> Values.empty t in
	  Map.add e (Values.add (Value.coerce t v) vs) m)
	bindings Map.empty

    let asuper_get e at =
      let t = Attribute_type.value_type at in
      Raw.asuper_get e (Attribute_type.(id (Ex at))) >|= fun bindings ->
      List.fold
	(fun (e, v) m ->
	  let vs = try Map.find e m with Not_found -> Values.empty t in
	  Map.add e (Values.add (Value.coerce t v) vs) m)
	bindings Map.empty

    let is_sub = Raw.is_sub
    let force_dsub = Raw.force_dsub
    let relax_dsub = Raw.relax_dsub
    let display_name ~langs e = Lwt.return ("#" ^ Int32.to_string e) (* TODO *)
  end
end
