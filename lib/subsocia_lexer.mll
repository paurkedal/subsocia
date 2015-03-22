(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

  let lexical_error lexbuf c =
    let pos = lexbuf.lex_start_p in
    fprintf stderr "%s:%d:%d: Not expecting '%c' here.\n%!" pos.pos_fname
	    pos.pos_lnum (pos.pos_cnum - pos.pos_bol) c
}

let space = [' ' '\t']
let bareedge = ['a'-'z' 'A'-'Z' '0'-'9' '_' '[' ']' '\x80'-'\xff']
let barefill = ['a'-'z' 'A'-'Z' '0'-'9' '_' '[' ']' '\x80'-'\xff'
		' ' '-' '.' ':' '@']
let barepath = bareedge (barefill* bareedge)?

rule lex_literal buf level = parse
  | '{' { Buffer.add_char buf '{'; lex_literal buf (level + 1) lexbuf }
  | '}' { if level = 0 then Buffer.contents buf else
	  (Buffer.add_char buf '}'; lex_literal buf (level - 1) lexbuf) }
  | [^ '{' '}']+ as s { Buffer.add_string buf s; lex_literal buf level lexbuf }

and lex = parse
  | '/' { SLASH }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | '+' { PLUS }
  | "*" space { CREATE }
  | "@" space { MODIFY }
  | "?@" space { DELETE }
  | '!' { ADDATTR }
  | '?' { DELATTR }
  | "?!" { SETATTR }
  | "!<" { ADDINCL }
  | "?<" { DELINCL }
  | '=' { EQ }
  | "={" { EQ_VERB (lex_literal (Buffer.create 80) 0 lexbuf) }
  | '#' { TOP }
  | '#' (['0'-'9']+ as s) { ID (Int32.of_string s) }
  | barepath as s { STR s }
  | space+ | '#' space [^ '\n']* { lex lexbuf }
  | '\n' { Lexing.new_line lexbuf; lex lexbuf }
  | eof { EOF }
  | _ as c { lexical_error lexbuf c; raise Parsing.Parse_error; }

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
