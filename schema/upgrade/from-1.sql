-- Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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
DELETE FROM subsocia.global_integer
  WHERE global_name = 'schema_version' AND global_value = 1;
----

ALTER TABLE subsocia.attribute_type ALTER COLUMN fts_config TYPE text;
ALTER TABLE subsocia.text_attribution_fts ALTER COLUMN fts_config TYPE text;

----
INSERT INTO subsocia.global_integer (global_name, global_value)
  VALUES ('schema_version', 2);
COMMIT;
