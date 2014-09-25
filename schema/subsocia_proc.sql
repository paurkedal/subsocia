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

CREATE OR REPLACE FUNCTION subsocia.upper_bounds(start_id integer)
  RETURNS TABLE (entity_id integer) AS
$$
BEGIN
  RETURN QUERY
    WITH RECURSIVE ub(entity_id) AS (
	SELECT superentity_id AS entity_id
	FROM subsocia.inclusion
	WHERE is_subsumed = false AND subentity_id = start_id
      UNION
	SELECT superentity_id AS entity_id
	FROM ub u JOIN subsocia.inclusion ON subentity_id = u.entity_id
	WHERE is_subsumed = false
    )
    SELECT u.entity_id FROM ub u;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION subsocia.lower_bounds(start_id integer)
  RETURNS TABLE (entity_id integer) AS
$$
BEGIN
  RETURN QUERY
    WITH RECURSIVE lb(entity_id) AS (
	SELECT subentity_id AS entity_id
	FROM subsocia.inclusion
	WHERE is_subsumed = false AND superentity_id = start_id
      UNION
	SELECT subentity_id AS entity_id
	FROM lb l JOIN subsocia.inclusion ON superentity_id = l.entity_id
	WHERE is_subsumed = false
    )
    SELECT l.entity_id FROM lb l;
END
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION subsocia.subsumed_by(sub_id integer, sup_id integer)
  RETURNS TABLE (subentity_id integer, superentity_id integer) AS
$$
BEGIN
  RETURN QUERY
    SELECT i.subentity_id, i.superentity_id
    FROM subsocia.inclusion i
    JOIN subsocia.lower_bounds(sub_id) AS l
      ON i.subentity_id = sub_id OR i.subentity_id = l.entity_id
    JOIN subsocia.upper_bounds(sup_id) AS u
      ON i.superentity_id = sup_id OR i.superentity_id = u.entity_id;
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
