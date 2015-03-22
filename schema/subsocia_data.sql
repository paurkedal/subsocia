-- Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version, with the OCaml static compilation exception.
--
-- This library is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
-- License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with this library.  If not, see <http://www.gnu.org/licenses/>.

-- Initial Data: Types

INSERT INTO subsocia.entity_type (entity_type_name, entity_name_tmpl) VALUES
  ('unit', DEFAULT),				-- 1
  ('org_group', DEFAULT),			-- 2
  ('access_base', DEFAULT),			-- 3
  ('access_role', DEFAULT),			-- 4
  ('auth_group', DEFAULT),			-- 5
  ('person', '${first_name} ${last_name}');	-- 6

INSERT INTO subsocia.attribute_type (attribute_name, value_type) VALUES
  ('unique_name', 'string'),	-- 1
  ('proper_name', 'string'),	-- 2
  ('first_name', 'string'),	-- 3
  ('last_name', 'string'),	-- 4
  ('email', 'string'),		-- 5
  ('role', 'string');		-- 6

INSERT INTO subsocia.inclusion_type
  (subentity_type_id, superentity_type_id,
   subentity_multiplicity, superentity_multiplicity)
VALUES
  (2, 1, 2, 0), -- org_group *--? unit
  (2, 2, 2, 0), -- org_group *--? org_group
  (3, 1, 2, 0), -- access_base *--? unit
  (4, 3, 2, 2), -- access_role *--* access_base
  (4, 4, 2, 2), -- access_role *--* access_role
  (5, 1, 2, 1), -- auth_group *--1 unit
  (6, 3, 2, 2), -- person *--* access_base
  (6, 4, 2, 2), -- person *--* access_role
  (6, 5, 2, 2); -- person *--* auth_group
INSERT INTO subsocia.attribution_type
  (subentity_type_id, superentity_type_id,
   attribute_type_id, attribute_multiplicity)
VALUES
  (2, 1, 1, 3), -- org_group [+:unique_name] unit
  (2, 2, 1, 3), -- org_group [+:unique_name] org_group
  (3, 1, 1, 3), -- access_base [+:unique_name] unit
  (3, 2, 1, 3), -- access_base [+:unique_name] org_group
  (3, 1, 2, 3), -- access_base [+:proper_name] unit
  (4, 3, 6, 2),	-- access_role [*:role] access_base
  (5, 2, 1, 3), -- auth_group [+:unique_name] org_group
  (6, 1, 2, 2), -- person [*:proper_name] unit
  (6, 1, 3, 1), -- person [1:first_name] unit
  (6, 1, 4, 1), -- person [1:last_name] unit
  (6, 1, 5, 2), -- person [*:email] unit
  (6, 3, 6, 2), -- person [*:role] access_base
  (6, 5, 1, 3); -- person [+:unique_name] auth_group

-- Initial Data: Objects

INSERT INTO subsocia.entity
  (entity_type_id, entity_rank, access_id)
VALUES
  (1, 0, 3), -- 1 top : unit; NB! Must have entity_id = 1
  (3, 0, 2), -- 2 forbidden : access_base
  (3, 0, 2), -- 3 default_access : access_base
  (4, 0, 3), -- 4 default_access/role=admin : access_role
  (2, 0, 3), -- 5 auth : org_group
  (3, 0, 3); -- 6 registrations : access_group

INSERT INTO subsocia.inclusion
  (subentity_id, superentity_id)
VALUES
  (2, 1), -- forbidden ⊆ top
  (3, 1), -- default_access ⊆ top
  (4, 3), -- default_access/role=admin ⊆ default_access
  (5, 1), -- auth ⊆ top
  (6, 1); -- registrations ⊆ top

INSERT INTO subsocia.text_attribution
  (subentity_id, superentity_id, attribute_type_id, value)
VALUES
  (1, 1, 1, 'top'),
  (2, 1, 1, 'forbidden'),
  (3, 1, 1, 'default_access'),
  (4, 3, 6, 'admin'),		-- default_access/role=admin
  (5, 1, 1, 'auth'),
  (6, 1, 1, 'registrations');
