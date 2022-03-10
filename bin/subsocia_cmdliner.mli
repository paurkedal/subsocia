(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
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

open Cmdliner
open Subsocia_common
open Subsocia_selector_types

module Arg : sig
  include module type of Cmdliner.Arg

  val ptime: Ptime.t Arg.conv

  val value_type : Type.any Arg.conv

  val multiplicity : Multiplicity.t Arg.conv

  val selector : selector Arg.conv

  val add_selector : add_selector Arg.conv
  val delete_selector : delete_selector Arg.conv

  val disable_transaction : bool Term.t
end
