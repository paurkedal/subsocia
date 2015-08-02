-- Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

DROP VIEW IF EXISTS subsocia.transitive_inclusion;
DROP VIEW IF EXISTS subsocia.transitive_reflexive_inclusion;

CREATE VIEW subsocia.transitive_inclusion (tsub_id, tsuper_id) AS
  WITH RECURSIVE inclusion_closure AS
    (
      SELECT dsub_id AS tsub_id, dsuper_id AS tsuper_id
	FROM subsocia.inclusion WHERE not is_subsumed
    UNION
      SELECT acc.tsub_id, i.dsuper_id AS tsuper_id
	FROM inclusion_closure AS acc JOIN subsocia.inclusion AS i
	  ON acc.tsuper_id = i.dsub_id
       WHERE not is_subsumed
    )
  SELECT * FROM inclusion_closure;

CREATE VIEW subsocia.transitive_reflexive_inclusion (tsub_id, tsuper_id) AS
  WITH RECURSIVE inclusion_closure AS
    (
      SELECT entity_id AS tsub_id, entity_id AS tsuper_id
	FROM subsocia.entity
    UNION
      SELECT acc.tsub_id, i.dsuper_id AS tsuper_id
	FROM inclusion_closure AS acc JOIN subsocia.inclusion AS i
	  ON acc.tsuper_id = i.dsub_id
       WHERE not is_subsumed
    )
  SELECT * FROM inclusion_closure;
