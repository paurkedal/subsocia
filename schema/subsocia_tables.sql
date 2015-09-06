-- Copyright (C) 2014--2015  Petter A. Urkedal <paurkedal@gmail.com>
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

CREATE SCHEMA subsocia;

CREATE TABLE subsocia.global_integer (
  global_name varchar(80) PRIMARY KEY,
  global_value integer NOT NULL
);
-- NB: Also update lib/subsocia_version.ml.ab.
INSERT INTO subsocia.global_integer VALUES ('schema_version', 2);

-- Types

CREATE TABLE subsocia.entity_type (
  entity_type_id SERIAL PRIMARY KEY,
  entity_type_name text UNIQUE NOT NULL,
  entity_name_tmpl text NOT NULL DEFAULT ('${unique_name}')
);
CREATE TABLE subsocia.inclusion_type (
  dsub_type_id integer NOT NULL REFERENCES subsocia.entity_type,
  dsub_mult smallint NOT NULL,
  dsuper_type_id integer NOT NULL REFERENCES subsocia.entity_type,
  dsuper_mult smallint NOT NULL,
  PRIMARY KEY (dsub_type_id, dsuper_type_id)
);

CREATE TABLE subsocia.attribute_type (
  attribute_type_id SERIAL PRIMARY KEY,
  attribute_name text UNIQUE NOT NULL,
  value_type text NOT NULL,
  fts_config text
);
CREATE SEQUENCE subsocia.attribute_uniqueness_id_seq;
CREATE TABLE subsocia.attribute_uniqueness (
  attribute_uniqueness_id integer NOT NULL,
  attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
  PRIMARY KEY (attribute_uniqueness_id, attribute_type_id)
);
CREATE TABLE subsocia.attribution_type (
  asub_type_id integer NOT NULL REFERENCES subsocia.entity_type,
  asuper_type_id integer NOT NULL REFERENCES subsocia.entity_type,
  attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
  attribute_mult smallint NOT NULL,
  PRIMARY KEY (attribute_type_id, asub_type_id, asuper_type_id)
);

-- Objects

CREATE TABLE subsocia.entity (
  entity_id SERIAL PRIMARY KEY,
  entity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
  entity_rank smallint NOT NULL DEFAULT 0,
  access_id integer REFERENCES subsocia.entity
);
CREATE TABLE subsocia.inclusion (
  dsub_id integer NOT NULL REFERENCES subsocia.entity ON DELETE CASCADE,
  dsuper_id integer NOT NULL REFERENCES subsocia.entity,
  is_subsumed boolean NOT NULL DEFAULT false,
  PRIMARY KEY (dsub_id, dsuper_id)
);
CREATE TABLE subsocia.attribution_bool (
  asub_id integer NOT NULL REFERENCES subsocia.entity ON DELETE CASCADE,
  asuper_id integer NOT NULL REFERENCES subsocia.entity,
  attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
  value boolean NOT NULL,
  PRIMARY KEY (asub_id, asuper_id, attribute_type_id, value)
);
CREATE TABLE subsocia.attribution_int (
  asub_id integer NOT NULL REFERENCES subsocia.entity ON DELETE CASCADE,
  asuper_id integer NOT NULL REFERENCES subsocia.entity,
  attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
  value integer NOT NULL,
  PRIMARY KEY (asub_id, asuper_id, attribute_type_id, value)
);
CREATE TABLE subsocia.attribution_string (
  asub_id integer NOT NULL REFERENCES subsocia.entity ON DELETE CASCADE,
  asuper_id integer NOT NULL REFERENCES subsocia.entity,
  attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
  value text NOT NULL,
  PRIMARY KEY (asub_id, asuper_id, attribute_type_id, value)
);
CREATE TABLE subsocia.attribution_string_fts (
  asub_id integer NOT NULL REFERENCES subsocia.entity ON DELETE CASCADE,
  asuper_id integer NOT NULL REFERENCES subsocia.entity ON DELETE CASCADE,
  fts_config text NOT NULL,
  fts_vector tsvector NOT NULL,
  PRIMARY KEY (asub_id, asuper_id, fts_config)
);
CREATE INDEX ON subsocia.attribution_string_fts USING gin(fts_vector);
