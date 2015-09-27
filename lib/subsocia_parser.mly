/* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
 */

%{
open Printf
open Subsocia_common
open Subsocia_schema_types
open Subsocia_selector_types

let parse_error msg =
  let pos = Parsing.symbol_start_pos () in
  let open Lexing in
  fprintf stderr "%s:%d:%d: %s\n%!" pos.pos_fname
	  pos.pos_lnum (pos.pos_cnum - pos.pos_bol) msg
%}

%token EOF CREATE MODIFY
%token AT_CREATE AT_DELETE
%token AU_FORCE AU_RELAX
%token ET_CREATE ET_CREATE_SIMPLE ET_MODIFY ET_DELETE ET_ALLOW ET_DISALLOW
%token E_CREATE E_DELETE E_FORCE E_RELAX
%token DELINCL ADDINCL ADDATTR DELATTR SETATTR
%token ARROW EQ LT LEQ GEQ SLASH COLON TOP MINUS PLUS COMMA
%token LBRACE RBRACE UNDERSCORE
%token<string> EQ_VERB STR STRING AUX_STRING AUX_SELECTOR
%token<int32> ID

%type<Subsocia_schema_types.schema> schema
%type<Subsocia_selector_types.selector> selector
%start selector schema
%%
schema: entries EOF { List.rev $1 };
entries:
    /* empty */ { [] }
  | entries entry { $2 :: $1 }
  ;

entry:
    CREATE STR create_constraints { `Create ($2, List.rev $3) }
  | MODIFY path modify_constraints { `Modify ($2, List.rev $3) }
  | AT_CREATE STR EQ STR { `At_create ($2, $4) }
  | AT_DELETE STR { `At_delete $2 }
  | AU_FORCE nonempty_string_list { `Au_force ($2) }
  | AU_RELAX nonempty_string_list { `Au_relax ($2) }
  | ET_CREATE STR et_create_constraints { `Et_create ($2, List.rev $3) }
  | ET_CREATE_SIMPLE STR { `Et_create ($2, []) }
  | ET_MODIFY STR et_modify_constraints { `Et_modify ($2, List.rev $3) }
  | ET_DELETE STR { `Et_delete $2 }
  | ET_ALLOW STR LT STR { `Et_allow_dsub ($2, $4) }
  | ET_DISALLOW STR LT STR { `Et_disallow_dsub ($2, $4) }
  | ET_ALLOW STR COLON STR ARROW STR { `Et_allow_attribution ($2, $4, $6) }
  | ET_DISALLOW STR COLON STR ARROW STR { `Et_disallow_attribution ($2,$4,$6) }
  | E_CREATE path COLON STR { `E_create ($2, $4) }
  | E_DELETE path { `Delete $2 }
  | E_FORCE path LT path { `E_force_dsub ($2, $4) }
  | E_RELAX path LT path { `E_relax_dsub ($2, $4) }
  | E_FORCE STR EQ STR COLON path ARROW path { `E_add_value ($2, $4, $6, $8) }
  | E_RELAX STR EQ STR COLON path ARROW path { `E_remove_value ($2,$4,$6,$8) }
  ;
et_create_constraints:
    /* empty */ { [] }
  | et_create_constraints et_create_constraint { $2 :: $1 }
  ;
et_modify_constraints:
    /* empty */ { [] }
  | et_modify_constraints et_modify_constraint { $2 :: $1 }
  ;
et_create_constraint:
    ADDINCL STR { `Allow_inclusion ($2, Multiplicity.May, Multiplicity.May) }
  | ADDATTR STR SLASH STR { `Allow_attribution ($2, $4) }
  | AUX_STRING STRING { `Aux_string ($1, $2) }
  ;
et_modify_constraint:
    ADDINCL STR { `Allow_inclusion ($2, Multiplicity.May, Multiplicity.May) }
  | ADDATTR STR SLASH STR { `Allow_attribution ($2, $4) }
  | DELINCL STR { `Disallow_inclusion $2 }
  | DELATTR STR SLASH STR { `Disallow_attribution ($2, $4) }
  | AUX_STRING STRING { `Aux_string ($1, $2) }
  ;
create_constraints:
    /* empty */ { [] }
  | create_constraints create_constraint { $2 :: $1 }
  ;
create_constraint:
    ADDINCL path { `Add_sub $2 }
  | ADDATTR path { `Add_attr $2 }
  ;
modify_constraints:
    /* empty */ { [] }
  | modify_constraints modify_constraint { $2 :: $1 }
  ;
modify_constraint:
    ADDINCL path { `Add_sub $2 }
  | DELINCL path { `Remove_sub $2 }
  | ADDATTR path { `Add_attr $2 }
  | DELATTR path { `Remove_attr $2 }
  | SETATTR path { `Set_attr $2 }
  | AUX_SELECTOR path { `Aux_selector ($1, $2) }
  ;

nonempty_string_list:
    STR { [$1] }
  | STR COMMA nonempty_string_list { $1 :: $3 }
  ;

selector: path EOF { $1 };
path:
    relative_path { $1 }
  | TOP { Select_top }
  | ID { Select_id $1 }
  | TOP relative_path { Select_with (Select_top, $2) }
  | ID relative_path { Select_with (Select_id $1, $2) }
  ;
relative_path:
    path_component { $1 }
  | relative_path SLASH path_component { Select_with ($1, $3) }
  ;
path_component:
    attribute { $1 }
  | attribute COLON STR { Select_type ($1, $3) }
  ;
attribute:
    disjunction { $1 }
  | STR EQ STR { Select_adjacent (Asub (Attribute_eq ($1, $3))) }
  | STR EQ_VERB { Select_adjacent (Asub (Attribute_eq ($1, $2))) }
  | STR EQ UNDERSCORE { Select_adjacent (Asub (Attribute_present $1)) }
  | STR LEQ STR { Select_adjacent (Asub (Attribute_leq ($1, $3))) }
  | STR GEQ STR { Select_adjacent (Asub (Attribute_geq ($1, $3))) }
  | MINUS STR EQ STR { Select_adjacent (Asuper (Attribute_eq ($2, $4))) }
  | MINUS STR EQ_VERB { Select_adjacent (Asuper (Attribute_eq ($2, $3))) }
  | MINUS STR EQ UNDERSCORE { Select_adjacent (Asuper (Attribute_present $2)) }
  | MINUS STR LEQ STR { Select_adjacent (Asuper (Attribute_leq ($2, $4))) }
  | MINUS STR GEQ STR { Select_adjacent (Asuper (Attribute_geq ($2, $4))) }
  ;
disjunction:
    disjunction_case { $1 }
  | disjunction COMMA disjunction_case { Select_union ($1, $3) }
  ;
disjunction_case:
    conjunction { $1 }
  | STR { Select_adjacent (Asub (Attribute_eq ("unique_name", $1))) }
  | PLUS { Select_adjacent Dsub }
  | MINUS { Select_adjacent Dsuper }
  | MINUS STR { Select_adjacent (Asuper (Attribute_eq ("unique_name", $2))) }
  ;
conjunction:
    braced { $1 }
  | conjunction braced { Select_inter ($1, $2) }
  ;
braced: LBRACE path RBRACE { $2 };
