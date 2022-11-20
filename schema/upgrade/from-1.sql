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
  WHERE global_name = 'schema_version' AND global_value = 1;
----

ALTER TABLE $.attribute_type ALTER COLUMN fts_config TYPE text;
ALTER TABLE $.text_attribution_fts ALTER COLUMN fts_config TYPE text;

DROP FUNCTION IF EXISTS $.subsumed_by(integer, integer);

ALTER TABLE $.integer_attribution RENAME TO attribution_int;
ALTER TABLE $.text_attribution RENAME TO attribution_string;
ALTER TABLE $.text_attribution_fts RENAME TO attribution_string_fts;

CREATE TABLE $.attribution_bool (
  asub_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  asuper_id integer NOT NULL REFERENCES $.entity,
  attribute_type_id integer NOT NULL REFERENCES $.attribute_type,
  value boolean NOT NULL,
  PRIMARY KEY (asub_id, asuper_id, attribute_type_id, value)
);

INSERT INTO $.attribution_bool
  SELECT asub_id, asuper_id, attribute_type_id, value <> 0 AS value
  FROM $.attribution_int NATURAL JOIN $.attribute_type
  WHERE value_type = 'bool';
DELETE FROM $.attribution_int
  USING $.attribute_type
  WHERE attribution_int.attribute_type_id = attribute_type.attribute_type_id
    AND value_type = 'bool';

----
INSERT INTO $.global_integer (global_name, global_value)
  VALUES ('schema_version', 2);
COMMIT;
