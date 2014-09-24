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

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let cache_hertz = Int64.to_float ExtUnixSpecific.(sysconf CLK_TCK)
let cache_second = 1.0 /. cache_hertz

let cache_section = Lwt_log.Section.make "subsocia.cache"

let cache_metric =
  let current_time () =
    let tms = Unix.times () in
    Unix.(tms.tms_utime +. tms.tms_stime) in
  let current_memory_pressure =
    fun () -> cache_hertz (* 1 GHz / 1 Gword *) in
  let report cs =
    let open Prime_cache_metric in
    Lwt_log.ign_debug_f ~section:cache_section
      "Beacon collection: time = %g; p = %g; n_live = %d; n_dead = %d"
      cs.cs_time cs.cs_memory_pressure
      cs.cs_live_count cs.cs_dead_count in
  Prime_cache_metric.create ~current_time ~current_memory_pressure ~report ()

module Beacon = Prime_beacon.Make (struct let cache_metric = cache_metric end)
