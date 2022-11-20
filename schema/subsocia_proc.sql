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

CREATE OR REPLACE FUNCTION $.max_subentity_rank(id integer)
  RETURNS integer AS
$$
BEGIN
  RETURN (
    SELECT COALESCE(max(entity_rank), -1)
      FROM $.entity JOIN $.inclusion
      ON entity_id = subentity_id WHERE superentity_id = id
  );
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  $.raise_rank(start_id integer, new_rank integer)
  RETURNS void AS
$$
DECLARE id integer;
BEGIN
  FOR id IN
    SELECT entity_id
      FROM $.inclusion JOIN $.entity ON superentity_id = entity_id
      WHERE subentity_id = start_id AND entity_rank <= new_rank
  LOOP
    PERFORM $.raise_rank(id, new_rank + 1);
  END LOOP;
  UPDATE $.entity SET entity_rank = new_rank WHERE entity_id = start_id;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  $.compress_rank(start_id integer, cur_rank integer)
  RETURNS void AS
$$
DECLARE tuple record;
        new_rank integer;
BEGIN
  new_rank := (SELECT $.max_subentity_rank(start_id)) + 1;
  IF new_rank != cur_rank THEN
    UPDATE $.entity SET entity_rank = new_rank
     WHERE entity_id = start_id;
    FOR tuple IN
      SELECT entity_id, entity_rank
        FROM $.inclusion JOIN $.entity
          ON superentity_id = entity_id
        WHERE subentity_id = start_id
          AND entity_rank > new_rank + 1
    LOOP
      PERFORM $.compress_rank(tuple.entity_id, tuple.entity_rank);
    END LOOP;
  END IF;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  $.insert_inclusion(sub_id integer, sup_id integer)
  RETURNS void AS
$$
DECLARE sub_rank integer;
BEGIN
  sub_rank :=
    (SELECT entity_rank FROM $.entity WHERE entity_id = sub_id);
  PERFORM $.raise_rank(sup_id, sub_rank + 1);
  INSERT INTO $.inclusion (subentity_id, superentity_id)
    VALUES (sub_id, sup_id);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  $.delete_inclusion(sub_id integer, sup_id integer)
  RETURNS void AS
$$
DECLARE sup_rank integer;
BEGIN
  DELETE FROM $.inclusion
    WHERE subentity_id = sub_id AND superentity_id = sup_id;
  sup_rank :=
    (SELECT entity_rank FROM $.entity WHERE entity_id = sup_id);
  PERFORM $.compress_rank(sup_id, sup_rank);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION $.reindex_fts() RETURNS void AS
$$
BEGIN
  DELETE FROM $.attribution_string_fts;
  INSERT INTO $.attribution_string_fts
    SELECT a.asub_id, a.asuper_id, at.fts_config,
           to_tsvector(at.fts_config::regconfig, string_agg(value, '$'))
    FROM $.attribution_string AS a
    NATURAL JOIN $.attribute_type AS at
    WHERE NOT at.fts_config IS NULL
    GROUP BY a.asub_id, a.asuper_id, at.fts_config;
END;
$$ LANGUAGE plpgsql;
