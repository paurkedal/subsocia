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
open Subsocia_parser
open Subsocia_selector_types
}

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
  | '=' { EQ }
  | "={" { EQ_VERB (lex_literal (Buffer.create 80) 0 lexbuf) }
  | '#' { TOP }
  | '#' (['0'-'9']+ as s) { ID (Int32.of_string s) }
  | [^ '{' '}' '/' '+' '=' '\n']+ as s { STR s }
  | eof { EOF }

{
  open Lexing

  let selector_of_string s =
    let lexbuf = from_string s in
    lexbuf.lex_curr_p <- {
      pos_fname = "";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    selector lex lexbuf
}
