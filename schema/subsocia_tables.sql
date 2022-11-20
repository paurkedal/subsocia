-- Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

CREATE SCHEMA $();

CREATE TABLE $.global_integer (
  global_name varchar(80) PRIMARY KEY,
  global_value integer NOT NULL
);
-- NB: Also update lib/subsocia_version.ml.ab.
INSERT INTO $.global_integer VALUES ('schema_version', 5);

-- Types

CREATE TABLE $.entity_type (
  entity_type_id SERIAL PRIMARY KEY,
  entity_type_name text UNIQUE NOT NULL,
  entity_name_tmpl text NOT NULL DEFAULT ('${unique_name}')
);
CREATE TABLE $.inclusion_type (
  dsub_type_id integer NOT NULL REFERENCES $.entity_type,
  dsub_mult smallint NOT NULL,
  dsuper_type_id integer NOT NULL REFERENCES $.entity_type,
  dsuper_mult smallint NOT NULL,
  PRIMARY KEY (dsub_type_id, dsuper_type_id)
);

CREATE TABLE $.attribute_type (
  attribute_type_id SERIAL PRIMARY KEY,
  attribute_name text UNIQUE NOT NULL,
  value_type text NOT NULL,
  value_mult smallint NOT NULL,
  fts_config text
);
CREATE SEQUENCE $.attribute_uniqueness_id_seq;
CREATE TABLE $.attribute_uniqueness (
  attribute_uniqueness_id integer NOT NULL,
  attribute_type_id integer NOT NULL REFERENCES $.attribute_type,
  PRIMARY KEY (attribute_uniqueness_id, attribute_type_id)
);
CREATE TABLE $.attribution_type (
  attribute_type_id integer NOT NULL REFERENCES $.attribute_type,
  domain_id integer NOT NULL REFERENCES $.entity_type,
  codomain_id integer NOT NULL REFERENCES $.entity_type,
  PRIMARY KEY (attribute_type_id, domain_id, codomain_id)
);

-- Objects

CREATE TABLE $.entity (
  entity_id SERIAL PRIMARY KEY,
  entity_type_id integer NOT NULL REFERENCES $.entity_type,
  entity_rank smallint NOT NULL DEFAULT 0
);
CREATE TABLE $.inclusion (
  dsub_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  dsuper_id integer NOT NULL REFERENCES $.entity,
  since timestamp NOT NULL DEFAULT (CURRENT_TIMESTAMP AT TIME ZONE 'UTC'),
  until timestamp,
  PRIMARY KEY (dsub_id, dsuper_id, since),
  CHECK (coalesce(since < until, true))
);
CREATE TABLE $.attribution_bool (
  input_id integer NOT NULL REFERENCES $.entity,
  output_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  attribute_type_id integer NOT NULL REFERENCES $.attribute_type,
  value boolean NOT NULL,
  PRIMARY KEY (input_id, output_id, attribute_type_id, value)
);
CREATE TABLE $.attribution_int (
  input_id integer NOT NULL REFERENCES $.entity,
  output_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  attribute_type_id integer NOT NULL REFERENCES $.attribute_type,
  value integer NOT NULL,
  PRIMARY KEY (input_id, output_id, attribute_type_id, value)
);
CREATE TABLE $.attribution_string (
  input_id integer NOT NULL REFERENCES $.entity,
  output_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  attribute_type_id integer NOT NULL REFERENCES $.attribute_type,
  value text NOT NULL,
  PRIMARY KEY (input_id, output_id, attribute_type_id, value)
);
CREATE TABLE $.attribution_string_fts (
  input_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  output_id integer NOT NULL REFERENCES $.entity ON DELETE CASCADE,
  fts_config text NOT NULL,
  fts_vector tsvector NOT NULL,
  PRIMARY KEY (input_id, output_id, fts_config)
);
CREATE INDEX ON $.attribution_string_fts USING gin(fts_vector);
