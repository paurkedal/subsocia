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

-- NOT IN USE

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
