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

type re = string
type fts = string

let at_soid_format : ('a, 'b, 'c, 'd) format4 = "#%ld"
let au_soid_format : ('a, 'b, 'c, 'd) format4 = "#%ld"
let et_soid_format : ('a, 'b, 'c, 'd) format4 = "#%ld"
let e_soid_format : ('a, 'b, 'c, 'd) format4 = "#%ld"
