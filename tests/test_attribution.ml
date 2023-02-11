(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open Lwt.Infix
open Lwt.Syntax
open Subsocia_common
open Subsocia_connection
open Subsocia_selector

let (%) f g x = f (g x)

module Alcotest = struct
  include Alcotest

  let entity_set =
    let pp ppf es =
      Fmt.pf ppf "(es : Entity.Set.t | cardinal es = %d)"
        (Entity.Set.cardinal es)
    in
    Alcotest.testable pp Entity.Set.equal
end

let completion words = Option.get (Subsocia_fts.of_completion_string words)

let test_image () =
  let* atB = Attribute_type.create Type.Bool "atB" in
  let* atI = Attribute_type.create Type.Int "atI" in
  let* atJ = Attribute_type.create Type.Int "atJ" in
  let* atK = Attribute_type.create Type.Int "atK" in
  let* atS = Attribute_type.create Type.String "atS" in
  let* atT = Attribute_type.create Type.String "atT" in

  let* etX = Entity_type.create "etX" in
  let* etY = Entity_type.create "etY" in
  let* etZ = Entity_type.create "etZ" in

  let* () = Entity_type.allow_attribution atB etX etY in (* X -{B}-> Y *)
  let* () = Entity_type.allow_attribution atI etX etY in (* X -{I}-> Y *)
  let* () = Entity_type.allow_attribution atJ etX etY in (* X -{J}-> Y *)
  let* () = Entity_type.allow_attribution atK etY etX in (* X -{K}-> Y *)
  let* () = Entity_type.allow_attribution atJ etY etZ in (* Y -{J}-> Z *)
  let* () = Entity_type.allow_attribution atK etZ etY in (* Z -{K}-> Y *)
  let* () = Entity_type.allow_attribution atS etX etY in (* X -{S}-> Y *)
  let* () = Entity_type.allow_attribution atT etX etY in (* X -{T}-> Y *)
  let* () = Entity_type.allow_attribution atS etY etZ in (* Y -{S}-> Z *)
  let* () = Entity_type.allow_attribution atS etZ etX in (* Z -{S}-> X *)

  let* eX1 = Entity.create etX in
  let* eX2 = Entity.create etX in
  let* eX3 = Entity.create etX in
  let* eY1 = Entity.create etY in
  let* eY2 = Entity.create etY in
  let* eY3 = Entity.create etY in
  let* eY4 = Entity.create etY in
  let* eY5 = Entity.create etY in
  let* eY6 = Entity.create etY in
  let* eZ1 = Entity.create etZ in

  let* () = Entity.add_value atI 1 eX1 eY1 in
  let* () = Entity.add_value atJ 2 eX1 eY2 in
  let* () = Entity.add_value atJ 3 eX1 eY3 in
  let* () = Entity.add_value atJ 4 eX1 eY4 in
  let* () = Entity.add_value atJ 5 eX1 eY5 in
  let* () = Entity.add_value atS "four" eX2 eY4 in
  let* () = Entity.add_value atS "six" eX2 eY6 in
  let* () = Entity.add_value atT "sIx" eX2 eY6 in
  let* () = Entity.add_value atK (-2) eY4 eX2 in
  let* () = Entity.add_value atK (-3) eY4 eX3 in
  let* () = Entity.add_value atS "one" eY4 eZ1 in

  let ck_img ?msg rel source expected_targets =
    let* msg =
      (match msg with
       | Some msg -> Lwt.return msg
       | None ->
          let+ selector = Relation.to_selector rel in
          ("image of " ^ string_of_selector selector))
    in
    let+ targets = Entity.image1 rel source in
    let expected_targets =
      Entity.Set.empty |> Prime_list.fold Entity.Set.add expected_targets
    in
    Alcotest.(check entity_set) msg
      expected_targets targets
  in
  ck_img Relation.true_ eX1 [eY1; eY2; eY3; eY4; eY5] >>= fun () ->
  ck_img Relation.(present atI) eX1 [eY1] >>= fun () ->
  ck_img Relation.(present atJ) eX1 [eY2; eY3; eY4; eY5] >>= fun () ->
  ck_img Relation.(atI = 1) eX1 [eY1] >>= fun () ->
  ck_img Relation.(atJ <:: [1; 3; 5]) eX1 [eY3; eY5] >>= fun () ->
  ck_img Relation.(atJ <= 3) eX1 [eY2; eY3] >>= fun () ->
  ck_img Relation.(atJ >= 3) eX1 [eY3; eY4; eY5] >>= fun () ->
  ck_img Relation.(between atJ 2 4) eX1 [eY2; eY3] >>= fun () ->
  ck_img Relation.(search atS (Subsocia_re.similar "s_x")) ~msg:"search"
    eX2 [eY6]

let test_cases = [
  Alcotest.test_case "image" `Quick (Lwt_main.run % test_image);
]
