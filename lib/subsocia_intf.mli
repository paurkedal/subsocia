(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

module type RO = sig

  type entity_id = int32
  type entity_type = int
  type entity

  val entity_id : entity -> entity_id
  val entity_type : entity -> entity_type
  val entity_viewer_id : entity -> entity_id
  val entity_admin_id : entity -> entity_id

  val fetch : entity_id -> entity Lwt.t
  val entity_viewer : entity -> entity Lwt.t
  val entity_admin : entity -> entity Lwt.t

  val min : unit -> entity list Lwt.t
  val max : unit -> entity list Lwt.t
  val pred : entity -> entity list Lwt.t
  val succ : entity -> entity list Lwt.t
  val preceq : entity -> entity -> bool Lwt.t

end

module type RW = sig
  include RO

  val create : entity_type: int -> viewer: entity -> admin: entity ->
	       unit -> entity Lwt.t

  val constrain : entity -> entity -> unit Lwt.t
  val unconstrain : entity -> entity -> unit Lwt.t
end
