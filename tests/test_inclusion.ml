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

open OUnit
open Subsocia_connection
open Printf
open Pwt_infix
open Unprime
open Unprime_option

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
    let len = String_set.fold (max *< String.length) keys 0 in
    keys |> String_set.iter @@ fun tag ->
      let tC, tW, n = get tag in
      if n = 0 then
        printf "%*s: %6d %8.3g %8.3g\n" len tag n tC tW
      else
        printf "%*s: %6d %8.3g %8.3g %8.3g %8.3g\n" len tag
               n tC tW (tC /. float_of_int n) (tW /. float_of_int n)
end

let test n =
  let%lwt root = Entity.root in
  let ea = Array.make n root in
  let ia = Array.init n (fun _ -> Array.make n false) in
  let%lwt org_group = Entity_type.of_name "org_group" >|= Option.get in
  Perf.start "insert";
  for%lwt i = 0 to n - 1 do
    let%lwt e = Entity.create org_group in
    ea.(i) <- e;
    for%lwt j = 0 to i - 1 do
      if Random.int 4 = 0 then begin
        ia.(i).(j) <- true;
        Perf.step "insert";
        Entity.force_dsub ea.(i) ea.(j)
      end else
        Lwt.return_unit
    done
  done >>
  Perf.stop_lwt "insert" >>
  Perf.start_lwt "update" >>
  for%lwt i = 0 to n - 1 do
    for%lwt j = 0 to i - 1 do
      if Random.int 4 = 0 then begin
        ia.(i).(j) <- not ia.(i).(j);
        Perf.step "update";
        if ia.(i).(j) then Entity.force_dsub ea.(i) ea.(j)
                      else Entity.relax_dsub ea.(i) ea.(j)
      end else
        Lwt.return_unit
    done
  done >>
  Perf.stop_lwt "update" >>
  Perf.start_lwt "closure" >>
  Lwt.return begin
    for i = 0 to n - 1 do
      for j = 0 to i - 1 do
        for k = 0 to j - 1 do
          if ia.(i).(j) && ia.(j).(k) then
            ia.(i).(k) <- true
        done
      done
    done
  end >>
  Perf.stop_lwt "closure" >>
  Perf.start_lwt "is_sub" >>
  for%lwt i = 0 to n - 1 do
    for%lwt j = 0 to i - 1 do
      Perf.step ~dn:2 "is_sub";
      let%lwt issub = Entity.is_sub ea.(i) ea.(j) in
      let%lwt issup = Entity.is_sub ea.(j) ea.(i) in
      let msg = sprintf "#%ld ⊆ #%ld" (Entity.id ea.(i)) (Entity.id ea.(j)) in
      assert_equal ~msg ~printer:string_of_bool ia.(i).(j) issub;
      assert (not issup);
      Lwt.return_unit
    done
  done >>
  Perf.stop_lwt "is_sub" >>
  Perf.start_lwt "delete" >>
  for%lwt i = n - 1 downto 0 do
    Perf.step "delete";
    Entity.delete ea.(i)
  done >>
  Perf.stop_lwt "delete" >>
  Lwt.return_unit

let run () = Lwt_main.run (test 100); Perf.show ()
