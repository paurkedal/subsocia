-- Copyright (C) 2020  Petter A. Urkedal <paurkedal@gmail.com>
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

BEGIN;

  -- Permanently drop procedures dealing with subsumption.
  --
  DROP FUNCTION IF EXISTS subsocia.subsumed(sub_id integer, super_id integer);
  DROP FUNCTION IF EXISTS subsocia.upper_bounds(start_id integer);
  DROP FUNCTION IF EXISTS subsocia.lower_bounds(start_id integer);

  -- Temporarily drop views which depends on is_subsumed.
  --
  DROP VIEW IF EXISTS subsocia.transitive_inclusion;
  DROP VIEW IF EXISTS subsocia.transitive_reflexive_inclusion;

  -- Drop is_subsumed.
  --
  ALTER TABLE subsocia.inclusion DROP COLUMN is_subsumed;

COMMIT;
