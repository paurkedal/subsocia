(* Copyright (C) 2015--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Subsocia_connection
open Printf
open Lwt.Infix
open Unprime

module String_set = Set.Make (String)

module Perf = struct
  let ht = Hashtbl.create 19

  let get tag = try Hashtbl.find ht tag with Not_found -> 0.0, 0.0, 0

  let step ?(dn = 1) tag =
    let tC, tW, n = get tag in
    Hashtbl.replace ht tag (tC, tW, n + dn)

  let start tag =
    let tC, tW, n = get tag in
    Hashtbl.replace ht tag (tC -. Sys.time (), tW -. Unix.time (), n)

  let stop tag =
    let tC, tW, n = get tag in
    Hashtbl.replace ht tag (tC +. Sys.time (), tW +. Unix.time (), n)

  let start_lwt tag = start tag; Lwt.return_unit
  let stop_lwt tag = stop tag; Lwt.return_unit

  let show () =
    let keys = Hashtbl.fold (fun s _ -> String_set.add s) ht String_set.empty in
    let len = String_set.fold (max % String.length) keys 0 in
    printf "%*s  %6s %8s %8s %8s %8s\n" len "" "n" "tC" "tW" "tC/n" "tW/n";
    keys |> String_set.iter @@ fun tag ->
      let tC, tW, n = get tag in
      if n = 0 then
        printf "%*s: %6d %8.3g %8.3g\n" len tag n tC tW
      else
        printf "%*s: %6d %8.3g %8.3g %8.3g %8.3g\n" len tag
               n tC tW (tC /. float_of_int n) (tW /. float_of_int n)
end

let test n =
  let sparseness = 16 in
  let%lwt root = Entity.get_root () in
  let ea = Array.make n root in
  let ia = Array.init n (fun _ -> Array.make n false) in
  let pp_entity =
    let get_index e = Option.get (Array.find_index (Entity.equal e) ea) in
    Fmt.(using get_index (any "x" ++ int))
  in
  let%lwt org_group = Entity_type.of_name_exn "org_group" in
  Perf.start "insert";
  for%lwt i = 0 to n - 1 do
    let%lwt e = Entity.create org_group in
    ea.(i) <- e;
    for%lwt j = 0 to i - 1 do
      if Random.int sparseness = 0 then begin
        ia.(i).(j) <- true;
        Perf.step "insert";
        Entity.force_dsub ea.(i) ea.(j)
      end else
        Lwt.return_unit
    done
  done >>= fun () ->
  Perf.stop_lwt "insert" >>= fun () ->
  Perf.start_lwt "update" >>= fun () ->
  for%lwt i = 0 to n - 1 do
    for%lwt j = 0 to i - 1 do
      if Random.int sparseness = 0 then begin
        ia.(i).(j) <- not ia.(i).(j);
        Perf.step "update";
        if ia.(i).(j) then Entity.force_dsub ea.(i) ea.(j)
                      else Entity.relax_dsub ea.(i) ea.(j)
      end else
        Lwt.return_unit
    done
  done >>= fun () ->
  Perf.stop_lwt "update" >>= fun () ->
  Perf.start_lwt "closure" >>= fun () ->
  Lwt.return begin
    for i = 0 to n - 1 do
      for j = 0 to i - 1 do
        for k = 0 to j - 1 do
          if ia.(i).(j) && ia.(j).(k) then
            ia.(i).(k) <- true
        done
      done
    done
  end >>= fun () ->
  Perf.stop_lwt "closure" >>= fun () ->
  for i = 0 to n - 1 do
    printf "%2d " i;
    for j = 0 to n - 1 do
      if j mod 10 = 0 then print_char '|';
      if ia.(i).(j) then printf "%d" (j mod 10) else print_char ' ';
    done;
    print_char '\n'
  done;
  Perf.start_lwt "is_sub" >>= fun () ->
  for%lwt i = 0 to n - 1 do
    for%lwt j = 0 to i - 1 do
      Perf.step ~dn:2 "is_sub";
      let%lwt issub = Entity.is_sub ea.(i) ea.(j) in
      let%lwt issup = Entity.is_sub ea.(j) ea.(i) in
      Alcotest.(check bool) (Fmt.str "e%d ⊆ e%d" i j) ia.(i).(j) issub;
      Alcotest.(check bool) (Fmt.str "e%d ⊇ e%d" i j) false issup;
      Lwt.return_unit
    done
  done >>= fun () ->
  Perf.stop_lwt "is_sub" >>= fun () ->
  Perf.start_lwt "super" >>= fun () ->
  for%lwt i = 0 to n - 1 do
    let%lwt super = Entity.super ea.(i) in
    Fmt.pr "super x%d = {@[%a@]}@." i Fmt.(list ~sep:comma pp_entity)
      (Entity.Set.elements super);
    for%lwt j = 0 to n - 1 do
      Alcotest.(check bool) (Fmt.str "x%d ∈ super x%d" j i) ia.(i).(j)
        (Entity.Set.mem ea.(j) super);
      Lwt.return_unit
    done
  done >>= fun () ->
  Perf.stop_lwt "super" >>= fun () ->
  Perf.start_lwt "sub" >>= fun () ->
  for%lwt i = 0 to n - 1 do
    let%lwt sub = Entity.sub ea.(i) in
    Fmt.pr "sub x%d = {@[%a@]}@." i Fmt.(list ~sep:comma pp_entity)
      (Entity.Set.elements sub);
    for%lwt j = 0 to n - 1 do
      Alcotest.(check bool) (Fmt.str "x%d ∈ sub x%d" j i) ia.(j).(i)
        (Entity.Set.mem ea.(j) sub);
      Lwt.return_unit
    done
  done >>= fun () ->
  Perf.stop_lwt "sub" >>= fun () ->
  Perf.start_lwt "delete" >>= fun () ->
  for%lwt i = n - 1 downto 0 do
    Perf.step "delete";
    Entity.delete ea.(i)
  done >>= fun () ->
  Perf.stop_lwt "delete" >>= fun () ->
  Lwt.return_unit

let test_cases = Alcotest.[
  test_case "random insert, update, is_sub" `Slow
    (fun () -> Lwt_main.run (test 64); Perf.show ());
]
