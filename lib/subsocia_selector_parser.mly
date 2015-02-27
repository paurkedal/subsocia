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
open Subsocia_selector_types
%}

%token EOF EQ SLASH TOP PLUS LBRACE RBRACE
%token<string> EQ_VERB STR
%token<int32> ID

%type<Subsocia_selector_types.selector> start
%start start
%%
start: path EOF { $1 };
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
