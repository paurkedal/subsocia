-- Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

-- Types

CREATE TABLE subsocia.entity_type (
    entity_type_id SERIAL PRIMARY KEY,
    entity_type_name text UNIQUE NOT NULL
);
CREATE TABLE subsocia.inclusion_type (
    subentity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    superentity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    subentity_multiplicity smallint NOT NULL,
    superentity_multiplicity smallint NOT NULL,
    PRIMARY KEY (subentity_type_id, superentity_type_id)
);

CREATE TABLE subsocia.attribute_type (
    attribute_type_id SERIAL PRIMARY KEY,
    attribute_name text UNIQUE NOT NULL,
    value_type text NOT NULL
);
CREATE TABLE subsocia.attribution_type (
    subentity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    superentity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
    attribute_multiplicity smallint NOT NULL,
    PRIMARY KEY (attribute_type_id, subentity_type_id, superentity_type_id)
);

-- Objects

CREATE TABLE subsocia.entity (
    entity_id SERIAL PRIMARY KEY,
    entity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    entity_rank smallint NOT NULL DEFAULT 0,
    viewer_id integer NOT NULL REFERENCES subsocia.entity,
    admin_id integer NOT NULL REFERENCES subsocia.entity
);
CREATE TABLE subsocia.inclusion (
    subentity_id integer NOT NULL REFERENCES subsocia.entity,
    superentity_id integer NOT NULL REFERENCES subsocia.entity,
    is_subsumed boolean NOT NULL DEFAULT false,
    PRIMARY KEY (superentity_id, subentity_id)
);
CREATE TABLE subsocia.integer_attribution (
    subentity_id integer NOT NULL REFERENCES subsocia.entity,
    superentity_id integer NOT NULL REFERENCES subsocia.entity,
    attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
    value integer NOT NULL,
    PRIMARY KEY (superentity_id, subentity_id, attribute_type_id)
);
CREATE TABLE subsocia.text_attribution (
    subentity_id integer NOT NULL REFERENCES subsocia.entity,
    superentity_id integer NOT NULL REFERENCES subsocia.entity,
    attribute_type_id integer NOT NULL REFERENCES subsocia.attribute_type,
    value text NOT NULL,
    PRIMARY KEY (superentity_id, subentity_id, attribute_type_id)
);
