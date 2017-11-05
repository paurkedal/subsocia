(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

open Printf
open Lwt.Infix
open Scanf
open Subsocia_common
open Subsocia_prereq
open Subsocia_rpc_types
open Unprime
open Unprime_list
open Unprime_option

module type RPCM = Subsocia_rpc_primitives.RPCM with type 'a t = 'a Lwt.t

module Attribute_type_base = struct
  type soid = int32
  type 'a t = {
    at_id : int32;
    at_name : string;
    at_type : 'a Type.t;
    at_mult : Multiplicity.t
  }
  type ex = Ex : 'a t -> ex
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

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.at_soid_format id
      let of_string s = sscanf s Subsocia_internal.at_soid_format (fun id -> id)
      let compare = compare
    end

    let of_soid at_id =
      let%lwt at_name, Type.Ex at_type, at_mult = Raw.of_soid at_id in
      Lwt.return (Ex {at_id; at_name; at_type; at_mult})
    let soid at = Lwt.return at.at_id

    let decode_set s = List.map (fun (Ex {at_id}) -> at_id) (Set.elements s)
    let encode_set ids = Lwt_list.map_s of_soid ids >|= Set.of_ordered_elements

    let of_name at_name =
      Raw.of_name at_name >|=
      Option.map (fun (at_id, Type.Ex at_type, at_mult) ->
                  Ex {at_id; at_name; at_type; at_mult})
    let name at = Lwt.return at.at_name
    let value_type at = at.at_type
    let value_mult at = at.at_mult
    let create ?(mult = Multiplicity.May) vt at_name =
      Raw.create (Type.Ex vt) mult at_name >|= fun at_id ->
      {at_id; at_name; at_type = vt; at_mult = mult}
    let delete at = Raw.delete at.at_id
    let all () = Raw.all () >>= encode_set

    (**/**)
    let of_id = of_soid
    let id at = at.at_id
  end

  module Attribute_uniqueness = struct
    module Raw = Raw.Attribute_uniqueness
    type soid = int32
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.au_soid_format id
      let of_string s = sscanf s Subsocia_internal.au_soid_format (fun id -> id)
      let compare = compare
    end

    exception Not_unique of Set.t

    let of_soid = Lwt.return
    let soid = Lwt.return

    let encode_set ids = Lwt_list.map_s of_soid ids >|= Set.of_ordered_elements

    let force s = Raw.force (Attribute_type.decode_set s)
    let relax u = Raw.relax u
    let find s = Raw.find (Attribute_type.decode_set s)
    let all () = Raw.all () >>= encode_set
    let affecting at = Raw.affecting (Attribute_type.id at) >>= encode_set
    let affected u = Raw.affected u >>= Attribute_type.encode_set

    (**/**)
    let of_id = Lwt.return
    let id u = u
  end

  module Relation = struct
    type t =
      | Inter : t list -> t
      | Present : 'a Attribute_type.t -> t
      | Eq : 'a Attribute_type.t * 'a -> t
      | In : 'a Attribute_type.t * 'a Values.t -> t
      | Leq : 'a Attribute_type.t * 'a -> t
      | Geq : 'a Attribute_type.t * 'a -> t
      | Between : 'a Attribute_type.t * 'a * 'a -> t
      | Search : string Attribute_type.t * string -> t
      | Search_fts : string -> t
  end

  module Entity_type = struct
    module Raw = Raw.Entity_type

    module Set = Int32_set
    module Map = Int32_map

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.et_soid_format id
      let of_string s = sscanf s Subsocia_internal.et_soid_format (fun id -> id)
      let compare = compare
    end

    type soid = int32
    type t = int32

    let of_name = Raw.of_name

    let of_soid = Lwt.return
    let soid = Lwt.return

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
      List.map (fun (et, muA, muB) -> et, (muA, muB)) %> Map.of_ordered_bindings

    let dsuper et =
      Raw.dsuper et >|=
      List.map (fun (et, muA, muB) -> et, (muA, muB)) %> Map.of_ordered_bindings

    let dsub_elements () =
      Raw.dsub_elements ()

    let allow_dsub mu0 mu1 et0 et1 =
      Raw.allow_dsub mu0 mu1 et0 et1

    let disallow_dsub et0 et1 =
      Raw.disallow_dsub et0 et1

    let can_attribute at et0 et1 =
      Raw.can_attribute (Attribute_type.id at) et0 et1

    let allowed_attributes lbt ubt =
      Raw.allowed_attributes lbt ubt >>=
      Lwt_list.map_s Attribute_type.of_soid >|=
      Attribute_type.Set.of_ordered_elements

    let allowed_preimage et =
      Raw.allowed_preimage et
        >>= Lwt_list.map_s
          (fun (et_id, at_ids) ->
            Lwt_list.map_s Attribute_type.of_soid at_ids >>= fun ats ->
            of_soid et_id >|= fun et ->
            (et, ats))
        >|= Map.of_ordered_bindings

    let allowed_image et =
      Raw.allowed_image et
        >>= Lwt_list.map_s
          (fun (et_id, at_ids) ->
            Lwt_list.map_s Attribute_type.of_soid at_ids >>= fun ats ->
            of_soid et_id >|= fun et ->
            (et, ats))
        >|= Map.of_ordered_bindings

    let allowed_mappings at =
      Raw.allowed_mappings at.Attribute_type.at_id

    let allowed_attributions () =
      let aux (at_id, et0, et1) =
        Attribute_type.of_soid at_id >|= fun at -> (at, et0, et1) in
      Raw.allowed_attributions () >>= Lwt_list.map_s aux

    let allow_attribution at et0 et1 =
      Raw.allow_attribution at.Attribute_type.at_id et0 et1

    let disallow_attribution at et0 et1 =
      Raw.disallow_attribution at.Attribute_type.at_id et0 et1

    (**/**)
    let id et = et
    let of_id et = Lwt.return et
  end

  module Entity = struct
    module Raw = Raw.Entity

    type soid = int32
    type t = int32

    module Set = Int32_set
    module Map = Int32_map

    module Soid = struct
      let to_string id = sprintf Subsocia_internal.e_soid_format id
      let of_string s = sscanf s Subsocia_internal.e_soid_format (fun id -> id)
      let compare = compare
    end

    let of_soid = Lwt.return
    let soid = Lwt.return

    let compare = compare

    let create = Raw.create
    let delete = Raw.delete
    let entity_type = Raw.entity_type
    let rank = Raw.rank
    let type_members et = Raw.type_members et >|= Set.of_ordered_elements
    let is_root e = Raw.is_root e
    let root = Raw.root ()
    let minimums () = Raw.minimums () >|= Set.of_ordered_elements
    let dsub ?et e = Raw.dsub et e >|= Set.of_ordered_elements
    let dsuper ?et e = Raw.dsuper et e >|= Set.of_ordered_elements

    let get_values at et0 et1 =
      let vt = Attribute_type.value_type at in
      Raw.get_values (Attribute_type.id at) et0 et1 >|=
      List.map (Value.coerce vt) %> Values.of_ordered_elements vt

    let check_uniqueness_error = function
      | [] -> Lwt.return_unit
      | ua_ids ->
        let%lwt uas = Lwt_list.map_s Attribute_uniqueness.of_soid ua_ids in
        Lwt.fail (Attribute_uniqueness.Not_unique
                    (Attribute_uniqueness.Set.of_ordered_elements uas))

    let add_values at vs e0 e1 =
      let vs = Values.elements vs in
      let t = Attribute_type.value_type at in
      Raw.add_values (Attribute_type.id at)
                     (List.map (fun v -> Value.Ex (t, v)) vs) e0 e1 >>=
      check_uniqueness_error

    let remove_values at vs e0 e1 =
      let vs = Values.elements vs in
      let t = Attribute_type.value_type at in
      Raw.remove_values (Attribute_type.id at)
                        (List.map (fun v -> Value.Ex (t, v)) vs) e0 e1

    let set_values at vs e0 e1 =
      let vs = Values.elements vs in
      let t = Attribute_type.value_type at in
      Raw.set_values (Attribute_type.id at)
                     (List.map (fun v -> Value.Ex (t, v)) vs) e0 e1 >>=
      check_uniqueness_error

    let rec encode_predicate = function
      | Relation.Inter ps -> Eap_inter (List.map encode_predicate ps)
      | Relation.Present at -> Eap_present (Attribute_type.id at)
      | Relation.Eq (at, x) ->
        let t = Attribute_type.value_type at in
        Eap_eq (Attribute_type.id at, Value.Ex (t, x))
      | Relation.In (at, vs) ->
        let t = Attribute_type.value_type at in
        Eap_in (Attribute_type.id at, Values.Ex (t, vs))
      | Relation.Leq (at, x) ->
        let t = Attribute_type.value_type at in
        Eap_leq (Attribute_type.id at, Value.Ex (t, x))
      | Relation.Geq (at, x) ->
        let t = Attribute_type.value_type at in
        Eap_geq (Attribute_type.id at, Value.Ex (t, x))
      | Relation.Between (at, x0, x1) ->
        let t = Attribute_type.value_type at in
        Eap_between (Attribute_type.id at, Value.Ex (t, x0), Value.Ex (t, x1))
      | Relation.Search (at, x) ->
        Eap_search (Attribute_type.id at, x)
      | Relation.Search_fts x ->
        Eap_search_fts x

    let image1 p e =
      Raw.image1 (encode_predicate p) e >|= Set.of_ordered_elements
    let preimage1 p e =
      Raw.preimage1 (encode_predicate p) e >|= Set.of_ordered_elements

    let asub_conj e ps = image1 (Relation.Inter ps) e
    let asuper_conj e ps = preimage1 (Relation.Inter ps) e

    let image1_eq at av e =
      let t = Attribute_type.value_type at in
      Raw.image1_eq (Attribute_type.id at) (Value.Ex (t, av)) e
        >|= Set.of_ordered_elements

    let preimage1_eq at av e =
      let t = Attribute_type.value_type at in
      Raw.preimage1_eq (Attribute_type.id at) (Value.Ex (t, av)) e
        >|= Set.of_ordered_elements

    let image1_fts = Raw.image1_fts
    let preimage1_fts = Raw.preimage1_fts

    let mapping1 at e =
      let t = Attribute_type.value_type at in
      Raw.mapping1 (Attribute_type.id at) e >|= fun bindings ->
      List.fold
        (fun (e, v) m ->
          let vs = try Map.find e m with Not_found -> Values.empty t in
          Map.add e (Values.add (Value.coerce t v) vs) m)
        bindings Map.empty

    let premapping1 at e =
      let t = Attribute_type.value_type at in
      Raw.premapping1 (Attribute_type.id at) e >|= fun bindings ->
      List.fold
        (fun (e, v) m ->
          let vs = try Map.find e m with Not_found -> Values.empty t in
          Map.add e (Values.add (Value.coerce t v) vs) m)
        bindings Map.empty

    let is_dsub = Raw.is_dsub
    let is_sub = Raw.is_sub
    let force_dsub = Raw.force_dsub
    let relax_dsub = Raw.relax_dsub
    let display_name ~langs e = Lwt.return ("#" ^ Int32.to_string e) (* TODO *)

    (**/**)
    let id e = e
    let of_id e = Lwt.return e
  end
end
