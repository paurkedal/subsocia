-- Copyright (C) 2020--2022  Petter A. Urkedal <paurkedal@gmail.com>
--
-- This library is free software; you can redistribute it and/or modify it
-- under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or (at your
-- option) any later version, with the LGPL-3.0 Linking Exception.
--
-- This library is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
-- License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- and the LGPL-3.0 Linking Exception along with this library.  If not, see
-- <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.

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

  -- Add period of validity to inclusions.
  --
  ALTER TABLE subsocia.inclusion
    ADD COLUMN since timestamp NOT NULL DEFAULT '1970-01-01T00:00:00Z',
    ADD COLUMN until timestamp;
  ALTER TABLE subsocia.inclusion
    ALTER COLUMN since SET DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'UTC');
  ALTER TABLE subsocia.inclusion DROP CONSTRAINT inclusion_pkey;
  ALTER TABLE subsocia.inclusion ADD PRIMARY KEY (dsub_id, dsuper_id, since);

  ALTER TABLE subsocia.inclusion ADD CHECK (coalesce(since < until, true));

  -- Set schema version.
  UPDATE subsocia.global_integer SET global_value = 5
   WHERE global_name = 'schema_version';

COMMIT;
