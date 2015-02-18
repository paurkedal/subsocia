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

INSERT INTO subsocia.entity_type (entity_type_name) VALUES
  ('unit'),		-- 1
  ('access_group'),	-- 2
  ('auth_group'),	-- 3
  ('person');		-- 4

INSERT INTO subsocia.attribute_type (attribute_name, value_type) VALUES
  ('unique_name', 'string'),	-- 1
  ('proper_name', 'string');	-- 2

INSERT INTO subsocia.inclusion_type
  (subentity_type_id, superentity_type_id,
   subentity_multiplicity, superentity_multiplicity)
VALUES
  (2, 1, 2, 0), -- access_group *--? unit
  (2, 2, 2, 2), -- access_group *--* access_group
  (3, 1, 2, 1), -- auth_group *--1 unit
  (4, 2, 2, 2), -- person *--* access_group
  (4, 3, 2, 2); -- person *--* auth_group
INSERT INTO subsocia.attribution_type
  (subentity_type_id, superentity_type_id,
   attribute_type_id, attribute_multiplicity)
VALUES
  (2, 1, 1, 1), -- access_group [1:unique_name] unit
  (2, 1, 2, 1), -- access_group [1:proper_name] unit
  (3, 1, 1, 1), -- auth_group [1:unique_name] unit
  (4, 1, 2, 3), -- person [+:proper_name] unit
  (4, 2, 1, 0); -- person [1:unique_name] auth_group

-- Initial Data: Objects

INSERT INTO subsocia.entity
  (entity_type_id, entity_rank, viewer_id, admin_id)
VALUES
  (1, 0, 3, 2), -- 1 All : unit
  (2, 0, 3, 2), -- 2 Forbidden : access_group
  (2, 0, 3, 4), -- 3 Default Viewers : access_group
  (2, 0, 3, 4), -- 4 Default Admins : access_group
  (3, 0, 3, 4); -- 5 Authenticated Users : auth_group

INSERT INTO subsocia.inclusion
  (subentity_id, superentity_id)
VALUES
  (2, 1), -- Forbidden -- All
  (3, 1), -- Default Viewers -- All
  (4, 3), -- Default Admins -- Default Viewers
  (5, 1); -- Authenticated Users -- All

INSERT INTO subsocia.text_attribution
  (subentity_id, superentity_id, attribute_type_id, value)
VALUES
  (1, 1, 1, 'All'),
  (2, 1, 1, 'Forbidden'),
  (3, 1, 2, 'Default Viewers'),
  (4, 1, 2, 'Default Admins');
