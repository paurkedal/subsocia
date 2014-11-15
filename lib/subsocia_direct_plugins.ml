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

open Subsocia_common
open Subsocia_direct
open Subsocia_prereq

module Group = struct

  let cn_info = {
    ai_key = Exists_attribute_key ("common_name", At_twine);
    ai_name = Twine.make [lang_of_string "en", "common name"];
  }

  module Q = struct
    let fetch = format_query
      "SELECT lang, common_name FROM @group_by_lang WHERE entity_id = ?"
  end

  module Make (Pool : CONNECTION_POOL) = struct

    let attributes = [cn_info]

    type id = int32

    type t = {
      id : id;
      common_name : Twine.t;
      beacon : Beacon.t;
    }
    let dummy = {
      id = Int32.zero;
      common_name = Twine.make [];
      beacon = Beacon.dummy;
    }

    module By_id = Weak.Make (struct
      type _t = t type t = _t
      let equal a b = a.id = b.id
      let hash a = Hashtbl.hash a.id
    end)
    let by_id = By_id.create 61

    let use_db f = Caqti_lwt.Pool.use f Pool.pool

    let fetch id =
      try Lwt.return (By_id.find by_id {dummy with id})
      with Not_found ->
	use_db @@ fun (module C : Caqti_lwt.CONNECTION) ->
	let decode tup acc = C.Tuple.(int 0 tup, text 1 tup) :: acc in
	lwt lss = C.fold Q.fetch decode C.Param.([|int32 id|]) [] in
	Lwt.return @@ Beacon.embed fetch_grade
	  (fun beacon -> {id; common_name = Twine.make lss; beacon})

    let fetch_display_name {common_name} ~langs =
      Lwt.return (Twine.to_string ~langs common_name)

    let fetch_attribute : type a. t -> a attribute_key -> a Lwt.t =
      fun o -> function
      | "common_name", At_twine -> Lwt.return o.common_name
      | _ -> raise Not_found

    let store_attribute : type a. t -> a attribute_key -> a -> unit Lwt.t =
      fun o -> function
      | "common_name", At_twine ->
	fun common_name -> assert false (* FIXME *)
      | _ -> raise Not_found
  end
end

module Person = struct

  let first_name_info = {
    ai_key = Exists_attribute_key ("first_name", At_string);
    ai_name = Twine.make [lang_of_string "en", "first name"];
  }
  let last_name_info = {
    ai_key = Exists_attribute_key ("last_name", At_string);
    ai_name = Twine.make [lang_of_string "en", "last name"];
  }

  module Q = struct
    let fetch = format_query
      "SELECT first_name, last_name FROM @person WHERE entity_id = ?"
    let store_first_name = format_query
      "UPDATE @person SET first_name = ? WHERE entity_id = ?"
    let store_last_name = format_query
      "UPDATE @person SET last_name = ? WHERE entity_id = ?"
  end

  module Make (Pool : CONNECTION_POOL) = struct

    let attributes = [first_name_info; last_name_info]

    type id = int32

    type t = {
      id : id;
      mutable first_name : string;
      mutable last_name : string;
      mutable updating : [`First_name | `Last_name] list;
      beacon : Beacon.t;
    }
    let dummy = {
      id = Int32.zero;
      first_name = "";
      last_name = "";
      updating = [];
      beacon = Beacon.dummy;
    }

    module By_id = Weak.Make (struct
      type _t = t type t = _t
      let equal a b = a.id = b.id
      let hash a = Hashtbl.hash a.id
    end)
    let by_id = By_id.create 61

    let use_db f = Caqti_lwt.Pool.use f Pool.pool

    let fetch id =
      try Lwt.return (By_id.find by_id {dummy with id})
      with Not_found ->
	use_db @@ fun (module C : Caqti_lwt.CONNECTION) ->
	let decode tup =
	  Beacon.embed fetch_grade @@ fun beacon ->
	    C.Tuple.({id; first_name = text 0 tup; last_name = text 1 tup;
		      updating = []; beacon}) in
	match_lwt C.find Q.fetch decode C.Param.([|int32 id|]) with
	| None -> Lwt.fail Not_found
	| Some e -> Lwt.return e

    let fetch_display_name {first_name; last_name} ~langs =
      Lwt.return (last_name ^ ", " ^ first_name)

    let fetch_attribute : type a. t -> a attribute_key -> a Lwt.t =
      fun o -> function
      | "first_name", At_string -> Lwt.return o.first_name
      | "last_name", At_string -> Lwt.return o.last_name
      | _ -> assert false

    let set_first_name o first_name =
      if first_name = o.first_name || List.mem `First_name o.updating then
	Lwt.return_unit
      else begin
	o.first_name <- first_name;
	o.updating <- `First_name :: o.updating;
	use_db @@ fun (module C : Caqti_lwt.CONNECTION) ->
	C.exec Q.store_first_name
	       C.Param.([|text first_name; int32 o.id|]) >|= fun () ->
	o.updating <- List.filter ((<>) `First_name) o.updating
      end

    let set_last_name o last_name =
      if last_name = o.last_name || List.mem `Last_name o.updating then
	Lwt.return_unit
      else begin
	o.last_name <- last_name;
	o.updating <- `Last_name :: o.updating;
	use_db @@ fun (module C : Caqti_lwt.CONNECTION) ->
	C.exec Q.store_last_name
	       C.Param.([|text last_name; int32 o.id|]) >|= fun () ->
	o.updating <- List.filter ((<>) `Last_name) o.updating
      end

    let store_attribute : type a. t -> a attribute_key -> a -> unit Lwt.t =
      fun o -> function
      | "first_name", At_string -> set_first_name o
      | "last_name", At_string -> set_last_name o
      | _ -> assert false
  end
end

let registered =
  register_entity_plugin "group" (module Group.Make);
  register_entity_plugin "person" (module Person.Make);
  ()
