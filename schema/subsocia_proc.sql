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


------------------------
-- Inclusion Triggers --
------------------------

DROP TRIGGER IF EXISTS pre_insert ON inclusion;
DROP TRIGGER IF EXISTS post_delete ON inclusion;

CREATE OR REPLACE FUNCTION
  $.inclusion_pre_insert_trigger() RETURNS trigger LANGUAGE plpgsql AS
$$
  BEGIN
    WITH RECURSIVE
      updated (id, new_rank) AS (
        SELECT e_sub.entity_id, e_super.entity_rank + 1
          FROM $.entity e_sub, $.entity e_super
          WHERE e_sub.entity_id = NEW.dsub_id
            AND e_super.entity_id = NEW.dsuper_id
            AND e_sub.entity_rank < e_super.entity_rank + 1
        UNION
        SELECT DISTINCT e_sub.entity_id, u.new_rank + 1
          FROM updated u
          JOIN $.inclusion i ON i.dsuper_id = u.id
          JOIN $.entity e_sub ON e_sub.entity_id = i.dsub_id
          WHERE e_sub.entity_rank < u.new_rank + 1
      )
      UPDATE $.entity SET entity_rank = u.new_rank
        FROM updated u WHERE entity_id = u.id;
    RETURN NEW;
  END
$$;

CREATE OR REPLACE FUNCTION
  $.compactify_rank(target_id integer) RETURNS void LANGUAGE plpgsql AS
$$
  DECLARE
    new_rank integer;
    rec record;
  BEGIN
    SELECT max(e_super.entity_rank) + 1 INTO STRICT new_rank
      FROM $.inclusion i
      JOIN $.entity e_super ON e_super.entity_id = i.dsuper_id
      WHERE i.dsub_id = target_id;
    UPDATE $.entity e SET entity_rank = new_rank
      WHERE e.entity_id = target_id
        AND e.entity_rank <> new_rank;
    IF FOUND THEN
      FOR rec IN
        SELECT i.dsub_id FROM $.inclusion i WHERE i.dsuper_id = target_id
      LOOP
        PERFORM $.compactify_rank(rec.dsub_id);
      END LOOP;
    END IF;
  END
$$;

CREATE OR REPLACE FUNCTION
  $.inclusion_post_delete_trigger() RETURNS trigger LANGUAGE plpgsql AS
$$
  BEGIN
    PERFORM $.compactify_rank(OLD.dsub_id);
    RETURN NULL;
  END
$$;


--------------------
-- FTS Reindexing --
--------------------

CREATE OR REPLACE FUNCTION
  $.reindex_fts() RETURNS void LANGUAGE plpgsql AS
$$
  BEGIN
    DELETE FROM $.attribution_string_fts;
    INSERT INTO $.attribution_string_fts
      SELECT
        a.asub_id, a.asuper_id, aty.fts_config,
        to_tsvector(aty.fts_config::regconfig, string_agg(value, '$(dollar)'))
      FROM $.attribution_string AS a
      NATURAL JOIN $.attribute_type AS aty
      WHERE NOT aty.fts_config IS NULL
      GROUP BY a.asub_id, a.asuper_id, aty.fts_config;
  END
$$;
