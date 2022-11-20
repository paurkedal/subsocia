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

ALTER TABLE $.global_integer ADD PRIMARY KEY (global_name);

DELETE FROM $.global_integer
 WHERE global_name = 'schema_version' AND global_value = 0;

ALTER TABLE $.inclusion_type
  RENAME COLUMN subentity_type_id TO dsub_type_id;
ALTER TABLE $.inclusion_type
  RENAME COLUMN superentity_type_id TO dsuper_type_id;
ALTER TABLE $.inclusion_type
  RENAME COLUMN subentity_multiplicity TO dsub_mult;
ALTER TABLE $.inclusion_type
  RENAME COLUMN superentity_multiplicity TO dsuper_mult;

ALTER TABLE $.attribution_type
  RENAME COLUMN subentity_type_id TO asub_type_id;
ALTER TABLE $.attribution_type
  RENAME COLUMN superentity_type_id TO asuper_type_id;
ALTER TABLE $.attribution_type
  RENAME COLUMN attribute_multiplicity to attribute_mult;

ALTER TABLE $.inclusion RENAME COLUMN subentity_id TO dsub_id;
ALTER TABLE $.inclusion RENAME COLUMN superentity_id TO dsuper_id;

ALTER TABLE $.integer_attribution
  RENAME COLUMN subentity_id TO asub_id;
ALTER TABLE $.integer_attribution
  RENAME COLUMN superentity_id TO asuper_id;
ALTER TABLE $.text_attribution
  RENAME COLUMN subentity_id TO asub_id;
ALTER TABLE $.text_attribution
  RENAME COLUMN superentity_id TO asuper_id;
ALTER TABLE $.text_attribution_fts
  RENAME COLUMN subentity_id TO asub_id;
ALTER TABLE $.text_attribution_fts
  RENAME COLUMN superentity_id TO asuper_id;

INSERT INTO $.global_integer (global_name, global_value)
 VALUES ('schema_version', 1);

COMMIT;
