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

CREATE OR REPLACE FUNCTION subsocia.upper_bounds(start_id integer)
  RETURNS TABLE (entity_id integer) AS
$$
BEGIN
  RETURN QUERY
    WITH RECURSIVE ub(entity_id) AS (
	SELECT start_id AS entity_id
      UNION
	SELECT dsuper_id AS entity_id
	FROM ub JOIN subsocia.inclusion ON dsub_id = ub.entity_id
	WHERE is_subsumed = false
    )
    SELECT ub.entity_id FROM ub;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION subsocia.lower_bounds(start_id integer)
  RETURNS TABLE (entity_id integer) AS
$$
BEGIN
  RETURN QUERY
    WITH RECURSIVE lb(entity_id) AS (
	SELECT start_id AS entity_id
      UNION
	SELECT dsub_id AS entity_id
	FROM lb JOIN subsocia.inclusion ON dsuper_id = lb.entity_id
	WHERE is_subsumed = false
    )
    SELECT lb.entity_id FROM lb;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION subsocia.subsumed(sub_id integer, super_id integer)
  RETURNS TABLE (dsub_id integer, dsuper_id integer) AS
$$
BEGIN
  RETURN QUERY
    SELECT i.dsub_id, i.dsuper_id
    FROM subsocia.inclusion AS i
    JOIN subsocia.upper_bounds(super_id) AS ub ON i.dsuper_id = ub.entity_id
    JOIN subsocia.lower_bounds(sub_id) AS lb ON i.dsub_id = lb.entity_id;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION subsocia.max_subentity_rank(id integer)
  RETURNS integer AS
$$
BEGIN
  RETURN (SELECT COALESCE(max(entity_rank), -1)
	  FROM subsocia.entity JOIN subsocia.inclusion
	  ON entity_id = subentity_id WHERE superentity_id = id);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  subsocia.raise_rank(start_id integer, new_rank integer)
  RETURNS void AS
$$
DECLARE id integer;
BEGIN
  FOR id IN
    SELECT entity_id
      FROM subsocia.inclusion JOIN subsocia.entity ON superentity_id = entity_id
      WHERE subentity_id = start_id AND entity_rank <= new_rank
  LOOP
    PERFORM subsocia.raise_rank(id, new_rank + 1);
  END LOOP;
  UPDATE subsocia.entity SET entity_rank = new_rank WHERE entity_id = start_id;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  subsocia.compress_rank(start_id integer, cur_rank integer)
  RETURNS void AS
$$
DECLARE tuple record;
	new_rank integer;
BEGIN
  new_rank := (SELECT subsocia.max_subentity_rank(start_id)) + 1;
  IF new_rank != cur_rank THEN
    UPDATE subsocia.entity SET entity_rank = new_rank
     WHERE entity_id = start_id;
    FOR tuple IN
      SELECT entity_id, entity_rank
	FROM subsocia.inclusion JOIN subsocia.entity
	  ON superentity_id = entity_id
	WHERE subentity_id = start_id
	  AND entity_rank > new_rank + 1
    LOOP
      PERFORM subsocia.compress_rank(tuple.entity_id, tuple.entity_rank);
    END LOOP;
  END IF;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  subsocia.insert_inclusion(sub_id integer, sup_id integer)
  RETURNS void AS
$$
DECLARE sub_rank integer;
BEGIN
  sub_rank := (SELECT entity_rank FROM subsocia.entity
	       WHERE entity_id = sub_id);
  PERFORM subsocia.raise_rank(sup_id, sub_rank + 1);
  INSERT INTO subsocia.inclusion (subentity_id, superentity_id)
    VALUES (sub_id, sup_id);
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION
  subsocia.delete_inclusion(sub_id integer, sup_id integer)
  RETURNS void AS
$$
DECLARE sup_rank integer;
BEGIN
  DELETE FROM subsocia.inclusion
    WHERE subentity_id = sub_id AND superentity_id = sup_id;
  sup_rank := (SELECT entity_rank FROM subsocia.entity
	       WHERE entity_id = sup_id);
  PERFORM subsocia.compress_rank(sup_id, sup_rank);
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION subsocia.reindex_fts() RETURNS void AS
$$
BEGIN
  DELETE FROM subsocia.attribution_string_fts;
  INSERT INTO subsocia.attribution_string_fts
    SELECT a.asub_id, a.asuper_id, at.fts_config,
	   to_tsvector(at.fts_config::regconfig, string_agg(value, '$'))
    FROM subsocia.attribution_string AS a NATURAL JOIN
	 subsocia.attribute_type AS at
    WHERE NOT at.fts_config IS NULL
    GROUP BY a.asub_id, a.asuper_id, at.fts_config;
END;
$$ LANGUAGE plpgsql;
