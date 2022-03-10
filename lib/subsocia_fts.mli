(* Copyright (C) 2015--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Full-text search patterns

    This is used for matching text attributes.

    This module is a stub to allow future refinement without breaking
    backwards compatibility. *)

type t = Subsocia_internal.fts

val tsquery : string -> t
(** Currently passed directly to PostgrSQL's `to_tsquery`. *)

val of_completion_string : string -> t option
(** Creates an FTS query from a space separated list of words where the last
    word is treated as a prefix.  [None] is returned if the string is considered
    too incomplete to create a useful query. *)
