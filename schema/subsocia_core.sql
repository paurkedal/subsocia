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

-- Core Relations

CREATE TABLE subsocia.entity (
    entity_id SERIAL PRIMARY KEY,
    entity_type smallint NOT NULL,
    entity_rank smallint NOT NULL DEFAULT 0,
    viewer_id integer NOT NULL REFERENCES subsocia.entity,
    admin_id integer NOT NULL REFERENCES subsocia.entity
);
CREATE TABLE subsocia.inclusion (
    superentity_id integer NOT NULL REFERENCES subsocia.entity,
    subentity_id integer NOT NULL REFERENCES subsocia.entity,
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
    superentity_id integer NOT NULL REFERENCES subsocia.entity,
    subentity_id integer NOT NULL REFERENCES subsocia.entity,
    new_state boolean NOT NULL
);

-- Auxiliary Data

CREATE TABLE subsocia.named_entity (
    entity_id integer NOT NULL REFERENCES subsocia.entity,
    entity_name text NOT NULL
);
