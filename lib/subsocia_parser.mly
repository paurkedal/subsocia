/* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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
open Subsocia_schema_types
open Subsocia_selector_types
%}

%token EOF EQ NOT_LT LT MINUS NL SLASH TOP PLUS LBRACE RBRACE
%token CREATE MODIFY DELETE
%token<string> EQ_VERB STR
%token<int32> ID

%type<Subsocia_schema_types.schema_entry list> schema
%type<Subsocia_selector_types.selector> selector
%start selector schema
%%
schema: entries EOF { List.rev $1 };
entries:
    /* empty */ { [] }
  | entries entry { $2 :: $1 }
  ;

entry:
    CREATE STR NL create_constraints { `Create ($2, List.rev $4) }
  | MODIFY path NL modify_constraints { `Modify ($2, List.rev $4) }
  | DELETE path NL { `Delete $2 }
  ;
create_constraints:
    /* empty */ { [] }
  | create_constraints create_constraint NL { $2 :: $1 }
  ;
create_constraint:
    LT path { `Add_sub $2 }
  | PLUS path { `Add_attr $2 }
  ;
modify_constraints:
    /* empty */ { [] }
  | modify_constraints modify_constraint NL { $2 :: $1 }
  ;
modify_constraint:
    LT path { `Add_sub $2 }
  | NOT_LT path { `Remove_sub $2 }
  | PLUS path { `Add_attr $2 }
  | MINUS path { `Remove_attr $2 }
  | EQ path { `Set_attr $2 }
  ;

selector: path EOF { $1 };
path:
    attribution { $1 }
  | path SLASH attribution { Select_sub ($1, $3) }
  ;
attribution:
    disjunction { $1 }
  | STR EQ STR { Select_attr ($1, $3) }
  | STR EQ_VERB { Select_attr ($1, $2) }
  | TOP { Select_top }
  | ID { Select_id $1 }
  | STR { Select_attr ("unique_name", $1) }
  | PLUS { Select_pred }
  ;
disjunction:
    conjunction { $1 }
  | disjunction PLUS conjunction { Select_union ($1, $3) }
  ;
conjunction:
    braced { $1 }
  | conjunction braced { Select_inter ($1, $2) }
  ;
braced: LBRACE path RBRACE { $2 };
