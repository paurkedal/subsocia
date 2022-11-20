-- Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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
DELETE FROM $.global_integer
  WHERE global_name = 'schema_version' AND global_value = 2;
----
ALTER TABLE $.attribute_type
  ADD COLUMN value_mult smallint NOT NULL DEFAULT 0;
ALTER TABLE $.attribute_type ALTER COLUMN value_mult DROP DEFAULT;

ALTER TABLE $.attribution_type DROP COLUMN attribute_mult;
ALTER TABLE $.attribution_type RENAME asuper_type_id TO domain_id;
ALTER TABLE $.attribution_type RENAME asub_type_id TO codomain_id;

ALTER TABLE $.attribution_bool RENAME asub_id TO output_id;
ALTER TABLE $.attribution_bool RENAME asuper_id TO input_id;
ALTER TABLE $.attribution_int RENAME asub_id TO output_id;
ALTER TABLE $.attribution_int RENAME asuper_id TO input_id;
ALTER TABLE $.attribution_string RENAME asub_id TO output_id;
ALTER TABLE $.attribution_string RENAME asuper_id TO input_id;
ALTER TABLE $.attribution_string_fts RENAME asub_id TO output_id;
ALTER TABLE $.attribution_string_fts RENAME asuper_id TO input_id;
----
INSERT INTO $.global_integer (global_name, global_value)
  VALUES ('schema_version', 3);
COMMIT;
