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
  WHERE global_name = 'schema_version' AND global_value = 2;
----
ALTER TABLE subsocia.attribute_type
  ADD COLUMN value_mult smallint NOT NULL DEFAULT 0;
ALTER TABLE subsocia.attribute_type ALTER COLUMN value_mult DROP DEFAULT;

ALTER TABLE subsocia.attribution_type DROP COLUMN attribute_mult;
ALTER TABLE subsocia.attribution_type RENAME asuper_type_id TO domain_id;
ALTER TABLE subsocia.attribution_type RENAME asub_type_id TO codomain_id;

ALTER TABLE subsocia.attribution_bool RENAME asub_id TO output_id;
ALTER TABLE subsocia.attribution_bool RENAME asuper_id TO input_id;
ALTER TABLE subsocia.attribution_int RENAME asub_id TO output_id;
ALTER TABLE subsocia.attribution_int RENAME asuper_id TO input_id;
ALTER TABLE subsocia.attribution_string RENAME asub_id TO output_id;
ALTER TABLE subsocia.attribution_string RENAME asuper_id TO input_id;
ALTER TABLE subsocia.attribution_string_fts RENAME asub_id TO output_id;
ALTER TABLE subsocia.attribution_string_fts RENAME asuper_id TO input_id;
----
INSERT INTO subsocia.global_integer (global_name, global_value)
  VALUES ('schema_version', 3);
COMMIT;
