-- Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

DROP VIEW IF EXISTS $.transitive_inclusion;
DROP VIEW IF EXISTS $.transitive_reflexive_inclusion;
DROP VIEW IF EXISTS $.attribution_present;

-- Transitive closure of the still valid subset of $.inclusion.
CREATE VIEW $.transitive_inclusion (tsub_id, tsuper_id) AS
  WITH RECURSIVE inclusion_closure AS (
    SELECT dsub_id AS tsub_id, dsuper_id AS tsuper_id
      FROM $.inclusion
    UNION SELECT DISTINCT acc.tsub_id, i.dsuper_id AS tsuper_id
      FROM inclusion_closure AS acc JOIN $.inclusion AS i
        ON acc.tsuper_id = i.dsub_id
  )
  SELECT * FROM inclusion_closure;

-- Transitive and reflexive closure of the still valid subset of
-- $.inclusion.
CREATE VIEW $.transitive_reflexive_inclusion (tsub_id, tsuper_id) AS
  WITH RECURSIVE inclusion_closure AS (
    SELECT entity_id AS tsub_id, entity_id AS tsuper_id
      FROM $.entity
    UNION SELECT DISTINCT acc.tsub_id, i.dsuper_id AS tsuper_id
      FROM inclusion_closure AS acc JOIN $.inclusion AS i
        ON acc.tsuper_id = i.dsub_id
  )
  SELECT * FROM inclusion_closure;

-- A combination of different attribution tables.
CREATE VIEW $.attribution_present (input_id, output_id, attribute_type_id) AS
  SELECT input_id, output_id, attribute_type_id FROM $.attribution_bool UNION
  SELECT input_id, output_id, attribute_type_id FROM $.attribution_int UNION
  SELECT input_id, output_id, attribute_type_id FROM $.attribution_string;

-- Display name cache

DROP MATERIALIZED VIEW IF EXISTS $.absolute_display_name;
CREATE MATERIALIZED VIEW $.absolute_display_name (entity_id, name) AS
  WITH RECURSIVE
    step (input_id, output_id, name, cost) AS (
      SELECT input_id, output_id, name, cost FROM (
        SELECT
            a.input_id,
            a.output_id,
            a.value AS name,
            t.display_cost AS cost,
            rank() OVER (
              PARTITION BY input_id, output_id ORDER BY t.display_cost
            ) AS pos
          FROM $.attribution_string a
          JOIN $.attribute_type t ON t.attribute_type_id = a.attribute_type_id
          WHERE t.display_cost IS NOT NULL
      ) AS tmp
      WHERE pos = 1
    ),
    absolute (entity_id, name, cost) AS (
      SELECT
          r.output_id AS entity_id,
          r.name,
          r.cost
        FROM step r WHERE r.input_id = 1
      UNION
      SELECT
          r.output_id AS entity_id,
          a.name || ' / ' || r.name AS name,
          a.cost + r.cost AS cost
        FROM step r, absolute a WHERE a.entity_id = r.input_id
    )
  SELECT DISTINCT
      entity_id,
      first_value(name) OVER (PARTITION BY entity_id ORDER BY cost)
    FROM absolute;
