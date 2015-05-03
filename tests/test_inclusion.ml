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

open OUnit
open Subsocia_connection
open Printf
open Pwt_infix
open Unprime
open Unprime_option

module String_set = Set.Make (String)

module Perf = struct
  let ht = Hashtbl.create 19

  let get tag = try Hashtbl.find ht tag with Not_found -> 0.0, 0.0

  let start tag =
    let tC, tW = get tag in
    Hashtbl.replace ht tag (tC -. Sys.time (), tW -. Unix.time ())

  let stop tag =
    let tC, tW = get tag in
    Hashtbl.replace ht tag (tC +. Sys.time (), tW +. Unix.time ())

  let start_lwt tag = start tag; Lwt.return_unit
  let stop_lwt tag = stop tag; Lwt.return_unit

  let show () =
    let keys = Hashtbl.fold (fun s _ -> String_set.add s) ht String_set.empty in
    let len = String_set.fold (max *< String.length) keys 0 in
    keys |> String_set.iter (fun tag -> let tC, tW = get tag in
					printf "%*s: %8g %8g\n" len tag tC tW)
end

let test n =
  lwt top = Entity.top in
  let ea = Array.make n top in
  let ia = Array.init n (fun _ -> Array.make n false) in
  lwt org_group = Entity_type.of_name "org_group" >|= Option.get in
  Perf.start "insert";
  for_lwt i = 0 to n - 1 do
    lwt e = Entity.create org_group in
    ea.(i) <- e;
    for_lwt j = 0 to i - 1 do
      if Random.int 4 = 0 then begin
	ia.(i).(j) <- true;
	Entity.force_dsub ea.(i) ea.(j)
      end else
	Lwt.return_unit
    done
  done >>
  Perf.stop_lwt "insert" >>
  Perf.start_lwt "update" >>
  for_lwt i = 0 to n - 1 do
    for_lwt j = 0 to i - 1 do
      if Random.int 4 = 0 then begin
	ia.(i).(j) <- not ia.(i).(j);
	if ia.(i).(j) then Entity.force_dsub ea.(i) ea.(j)
		      else Entity.relax_dsub ea.(i) ea.(j)
      end else
	Lwt.return_unit
    done
  done >>
  Perf.stop_lwt "update" >>
  Perf.start_lwt "closure" >>
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      for k = 0 to j - 1 do
	if ia.(i).(j) && ia.(j).(k) then
	  ia.(i).(k) <- true
      done
    done
  done;
  Perf.stop_lwt "closure" >>
  Perf.start_lwt "is_sub" >>
  for_lwt i = 0 to n - 1 do
    for_lwt j = 0 to i - 1 do
      lwt issub = Entity.is_sub ea.(i) ea.(j) in
      lwt issup = Entity.is_sub ea.(j) ea.(i) in
      let msg = sprintf "#%ld ⊆ #%ld" (Entity.id ea.(i)) (Entity.id ea.(j)) in
      assert_equal ~msg ~printer:string_of_bool ia.(i).(j) issub;
      assert (not issup);
      Lwt.return_unit
    done
  done >>
  Perf.stop_lwt "is_sub" >>
  Perf.start_lwt "delete" >>
  for_lwt i = n - 1 downto 0 do
    Entity.delete ea.(i)
  done >>
  Perf.stop_lwt "delete" >>
  Lwt.return_unit

let run () = Lwt_main.run (test 100); Perf.show ()
