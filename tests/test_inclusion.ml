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
open Unprime_option

let test n =
  lwt top = Entity.top in
  let ea = Array.make n top in
  let ia = Array.init n (fun _ -> Array.make n false) in
  lwt org_group = Entity_type.of_name "org_group" >|= Option.get in
  for_lwt i = 0 to n - 1 do
    lwt e = Entity.create org_group in
    ea.(i) <- e;
    for_lwt j = 0 to i - 1 do
      if Random.int 4 = 0 then begin
	ia.(i).(j) <- true;
	Entity.constrain ea.(i) ea.(j)
      end else
	Lwt.return_unit
    done
  done >>
  for_lwt i = 0 to n - 1 do
    for_lwt j = 0 to i - 1 do
      if Random.int 4 = 0 then begin
	ia.(i).(j) <- not ia.(i).(j);
	if ia.(i).(j) then Entity.constrain ea.(i) ea.(j)
		      else Entity.unconstrain ea.(i) ea.(j)
      end else
	Lwt.return_unit
    done
  done >>
  for i = 0 to n - 1 do
    for j = 0 to i - 1 do
      for k = 0 to j - 1 do
	if ia.(i).(j) && ia.(j).(k) then
	  ia.(i).(k) <- true
      done
    done
  done;
  for_lwt i = 0 to n - 1 do
    for_lwt j = 0 to i - 1 do
      lwt issub = Entity.precedes ea.(i) ea.(j) in
      lwt issup = Entity.precedes ea.(j) ea.(i) in
      let msg = sprintf "#%ld ⊆ #%ld" (Entity.id ea.(i)) (Entity.id ea.(j)) in
      assert_equal ~msg ~printer:string_of_bool ia.(i).(j) issub;
      assert (not issup);
      Lwt.return_unit
    done
  done >>
  for_lwt i = n - 1 downto 0 do
    Entity.delete ea.(i)
  done

let run () = Lwt_main.run (test 100)
