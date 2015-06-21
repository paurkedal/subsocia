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

(** Regular expression style patterns

    This is used for matching text attributes.

    Currently this is just a stub to allow future refinement without breaking
    backwards compatibility.  In the future we may need to translate the
    result to different regular expression implementations for different
    RDBMs, and we may want to provide an `ocaml-re`-like interface. *)

type t = Subsocia_internal.re

val similar : string -> t
(** [similar s] matches the same strings as SQL:1999 `SIMILAR TO s`. *)
