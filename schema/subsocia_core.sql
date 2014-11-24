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
    entity_type_name text UNIQUE NOT NULL,
    entity_plugin text NOT NULL
);
CREATE TABLE subsocia.entity_type_by_lang (
    entity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    lang integer NOT NULL,
    display_name text NOT NULL,
    display_name_pl text,
    PRIMARY KEY (entity_type_id, lang)
);
CREATE TABLE subsocia.inclusion_type (
    subentity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    superentity_type_id integer NOT NULL REFERENCES subsocia.entity_type,
    subentity_multiplicity smallint NOT NULL,
    superentity_multiplicity smallint NOT NULL,
    UNIQUE (subentity_type_id, superentity_type_id)
);

-- Core Relations

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
    UNIQUE (superentity_id, subentity_id)
);

-- Change Log

CREATE TABLE subsocia.entity_log (
    edit_time timestamp NOT NULL DEFAULT current_timestamp,
    edit_author_id integer NOT NULL REFERENCES subsocia.entity,
    edit_note text,
    entity_id integer NOT NULL REFERENCES subsocia.entity,
    new_entity_type smallint,
    new_viewer_id integer REFERENCES subsocia.entity,
    new_admin_id integer REFERENCES subsocia.entity
);
CREATE TABLE subsocia.inclusion_log (
    edit_time timestamp NOT NULL DEFAULT current_timestamp,
    edit_author_id integer NOT NULL REFERENCES subsocia.entity,
    edit_note text,
    subentity_id integer NOT NULL REFERENCES subsocia.entity,
    superentity_id integer NOT NULL REFERENCES subsocia.entity,
    new_state boolean NOT NULL
);

-- Auxiliary Data

CREATE TABLE subsocia.auth_identity (
    entity_id integer PRIMARY KEY REFERENCES subsocia.entity,
    auth_method text NOT NULL,
    auth_identity text NOT NULL,
    auth_attributes text,
    first_seen timestamp NOT NULL DEFAULT current_timestamp,
    last_seen timestamp,
    UNIQUE (auth_method, auth_identity)
);
CREATE TABLE subsocia.person (
    entity_id integer PRIMARY KEY REFERENCES subsocia.entity,
    first_name text NOT NULL,
    last_name text NOT NULL,
    email text
);
CREATE TABLE subsocia.group_by_lang (
    entity_id integer NOT NULL REFERENCES subsocia.entity,
    lang integer NOT NULL,
    common_name text NOT NULL,
    PRIMARY KEY (entity_id, lang)
);
