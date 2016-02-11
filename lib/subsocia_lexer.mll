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

{
  open Lexing
  open Printf
  open Subsocia_parser
  open Subsocia_selector_types

  let lexical_error lexbuf s =
    let pos = lexbuf.lex_start_p in
    fprintf stderr "%s:%d:%d: Not expecting '%s' here.\n%!" pos.pos_fname
            pos.pos_lnum (pos.pos_cnum - pos.pos_bol) s

  let keywords = Hashtbl.create 19
  let () = Array.iter (fun (kw, token) -> Hashtbl.add keywords kw token)
    [|
      "attribute_type", AT_CREATE;
      "drop_attribute_type", AT_DELETE;
      "unique", AU_FORCE;
      "drop_unique", AU_RELAX;
      "entity_type", ET_CREATE_SIMPLE;
      "drop_entity_type", ET_DELETE;
      "allow", ET_ALLOW;
      "disallow", ET_DISALLOW;
      "display_template", ET_DISPLAY;
      "entity", E_CREATE;
      "drop_entity", E_DELETE;
      "force", E_FORCE;
      "relax", E_RELAX;
    |]
}

let space = [' ' '\t']
let identifier = ['A'-'Z' 'a'-'z' '_'] ['0'-'9' 'A'-'Z' 'a'-'z' '_']*
let bareedge = ['a'-'z' 'A'-'Z' '0'-'9' '_' '[' ']' '\x80'-'\xff']
let barefill = ['a'-'z' 'A'-'Z' '0'-'9' '_' '[' ']' '\x80'-'\xff'
                ' ' '-' '.' '@']
let barepath = bareedge (barefill* bareedge)?

rule lex_literal buf level = parse
  | '{' { Buffer.add_char buf '{'; lex_literal buf (level + 1) lexbuf }
  | '}' { if level = 0 then Buffer.contents buf else
          (Buffer.add_char buf '}'; lex_literal buf (level - 1) lexbuf) }
  | [^ '{' '}']+ as s { Buffer.add_string buf s; lex_literal buf level lexbuf }

and lex_string buf = parse
  | '"' { STRING (Buffer.contents buf) }
  | '\\' (_ as c) { Buffer.add_char buf c; lex_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; lex_string buf lexbuf }

and lex = parse
  | '/' { SLASH }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }
  | '-' { MINUS }
  | '+' { PLUS }
  | ',' { COMMA }
  | '_' { UNDERSCORE }
  | ":" { COLON }
  | ":" (identifier as idr) { TYPE_FILTER idr }
  | ":!" space { AT_CREATE }
  | ":?" space { AT_DELETE }
  | ":*" space { ET_CREATE }
  | ":@" space { ET_MODIFY }
  | ":@?" space { ET_DELETE }
  | "*" space { CREATE }
  | "@" space { MODIFY }
  | "@?" space { E_DELETE }
  | '%' (identifier as idr)
    { match idr with
      | "access" -> AUX_SELECTOR idr
      | "display" | "tsconfig" -> AUX_STRING idr
      | _ ->
        try Hashtbl.find keywords idr with Not_found ->
        lexical_error lexbuf idr; raise Parsing.Parse_error }
  | '!' { ADDATTR }
  | '?' { DELATTR }
  | "?!" { SETATTR }
  | "!<" { ADDINCL }
  | "?<" { DELINCL }
  | "->" { ARROW }
  | '=' { EQ }
  | "<" { LT }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "={" { EQ_VERB (lex_literal (Buffer.create 80) 0 lexbuf) }
  | '"' { lex_string (Buffer.create 80) lexbuf }
  | "#" { TOP }
  | '#' (['0'-'9']+ as s) { ID (Int32.of_string s) }
  | barepath as s { STR s }
  | space+ | '#' space [^ '\n']* { lex lexbuf }
  | '#'? '\n' { Lexing.new_line lexbuf; lex lexbuf }
  | eof { EOF }
  | _ as c
    { lexical_error lexbuf (String.make 1 c); raise Parsing.Parse_error; }

{
  let selector_of_string s =
    let lexbuf = from_string s in
    lexbuf.lex_curr_p <- {
      pos_fname = "";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    selector lex lexbuf

  let parse_schema fp =
    Prime_io.with_file_in
      begin fun ic ->
        let lexbuf = from_channel ic in
        lexbuf.lex_curr_p <- {
          pos_fname = fp;
          pos_lnum = 1;
          pos_bol = 0;
          pos_cnum = 0
        };
        schema lex lexbuf
      end
      fp
}
