## v0.23.2 - 2025-12-15

  - Fix `Entity.connected_by`.

## v0.23.1 - 2025-11-19

  - Added `is-sub` subcommand.
  - Fixed lower time check for `Entity.sub` and `Entity.super`.
  - Dropped rank optimisation and caching for `Entity.is_sub`.

## v0.23.0 - 2025-11-03

  - New functions: `Entity.super`, `Entity.sub`, `Entity.image`,
    `Entity.preimage`.

## v0.22.0 - 2025-10-06

  - Fixed `Subsocia_direct.transaction` and added missing transaction scopes
    to `subsocia` subcommands which modify the database.
  - The `subsocia delete` command now has `-d` and `-r` options to allow
    stripping inclusions and attributes in the same operation that the
    entity is deleted.
  - The enforcement time for inclusions can now be specified in schema files
    with the new `%from` attribute after inclusion constraints.  The default
    enforcement time can now also be provided by passing `--time` to the
    `load`, `create`, and `db-init` subcommands.

## v0.21.0 - 2023-09-19

  - Upgrade to Caqti 2.0.

## v0.20.0 - 2023-03-28

Library:

  - Switch configuration to JSON with Yojson extensions.
  - Extend the entity interface with `unique_mapping1`,
    `unique_relations_by_image`, and `unique_relations_by_preimage`.
  - Add `Relation.True`.
  - Use `logs` instead of `lwt_log`.
  - Use SQL triggers for maintaining ranks (schema version 6).
  - Register pretty-printer for exception.

Utility:

  - Remove space at end of line for in-list output.
  - Add `--disable-core-schema` to `db-init` subcommand. The root entity
    will still be created, as this is now done by the SQL schema.
  - Fix misleading metavariables in an-allow and an-disallow documentation.

Internal:

  - Various test improvements.
  - Replace `Caqti_lwt_sql_io` with Angstrom parser.
  - Fix other deprecations and breakage for recent caqti, cmdliner, extunix.
  - Simplify select for `is_sub` when `?time` provided.

The project license now uses the LGPL-3.0 Linking Exception.

## v0.19.1 - 2020-06-19

  - Fix queries for `force_dsub` and `dsub_history`.

## v0.19.0 - 2020-06-11

  - Historic inclusions are now kept permanently with timestamps indicating
    periods of validity.  In addition to SQL an schema update this includes
    changes to the API and command line tool to allow probing inclusions at
    a given time or providing the full inclusion history of an entity.
  - The optimisation of recording subsumption of inclusion in the DB is
    dropped.
  - Extended transactional signature to `Subsocia_intf.S_SOID`.
  - Fixed inversion of exit code for bool-returning subcommands.
  - Improved documentation of entity-subcommands.

## v0.18.2 - 2020-01-06

- Fix Caqti 1.2 compatibility, broken due to internal concrete type alias.
- Add `Entity.connected_by`.

## v0.18.1 - 2019-01-24

- Move `lwt_log` and extunix dependencies to subsocia.data.

## v0.18.0 - 2019-01-24

Breaking changes:

- The web interface has been moved to a separate project subsocia-eliom with
  corresponding changes to findlib names.
- `Subsocia_intf.ATTRIBUTE_TYPE.ex` has been replaced by `any` with
  corresponding changes to some function signatures and the `Set` and `Map`
  modules.
- The `coerce`, `required`, and `typed_required` from
  `Subsocia_derived_intf.ATTRIBUTE_TYPE` has
  been replaced by `coerce_any`, `any_of_name_exn`, `of_name_exn` from
  `Subsocia_intf.ATTRIBUTE_TYPE`.
- `Subsocia_intf.ATTRIBUTE_TYPE.of_name` has been replaced by `of_name_exn`.
- `Failure` exceptions have been replaced by `Subsocia_error.Exn`.
- Removed various deprecated functions.
- Replaced `entity_changed` event table with plain callback registration
  `on_entity_change`.
- Some changes to the semi-internal API of `Subsocia_direct`.

Non-breaking changes:

- Added `equal` and `compare` to `Type` and `Values`.
- Added `Type.pp`.
- Added module `Caqti_error`.
- Fix cache clearing after transactions.
- Parametrised DB schema prefix.
- Use `iso639` package for nationalization. Default language codes are now 3
  letter, but falling back to 2 letter lookups for compatibility.

## v0.17.1 - 2018-06-21

- Fix loop termination in init.db.
- Fix SQL code affecting insertion of attribute uniqueness.
- Fix insert and delete for bool attributes.
- Fix caqti-dynload package dependency.
- Add function to clear DB caches.
- Remove newlines from SQL code.
- Deprecate module-level constants.
- Deprecate Value.ex and rename ex to any in Type and Values.

## v0.17.0 - 2018-02-27

- Removed RPC API.
- Update to Lwt 3.
- Fix a injection of exception into lwt threads.

## v0.16.1 - 2018-02-19

- Update to work with Eliom 6.3.0 and Caqti 0.10.0.

## v0.16.0 - 2018-02-06

- Convert to Caqti v2.
- Rewrite plugin system to use findlib directly.

## v0.15.1 - 2017-12-06

- Fix deprecations for Caqti 0.8.0 and use future-proof findlib names.

## v0.15.0 - 2017-06-05

- Update to Caqti 0.6.0.

## v0.14.0 - 2016-12-15

- Update to Eliom 6.

## v0.13.3 - 2016-11-12

- Fix server-client typing.

## v0.13.2 - 2016-09-22

- Added et-info subcommand.
- Fixed filter construction for full-text search.

## v0.13.1 - 2016-08-16

### Added
- Added `Soid.compare`.

### Fixed
- Added missing yojson (and implied uri) dependency.
- Fix escape condition and root case in printer, and tweak lexer.

### Removed
- `Subsocia_derived_intf.S.Base` component, indented only for internal use.

## v0.13.0 - 2016-08-14

### Added
- JSON conversions for values.
- Added `Entity.is_root`
- Added `Entity_type.allowed_preimage`, `Entity_type.allowed_image`.
- Added `Entity.unique_premapping1`
- Added `Values.choose`.
- Added `Relation.to_selector`.

### Changed
- Generalized transaction type and removed an unused type.
- Added `soid` and `of_soid` functions and `Soid` module for AT, AU, ET, and
  E and deprecated `id` and `of_id`.
- Dropped `#` prefix from start of rooted selectors.

### Fixed
- Rewrote `Entity.paths` to use attribute uniqueness and fixing
  constructions including non-refinement attributes.
- Fixed parsing of `#` and `#n` as selector prefixes.

### Removed
- Removed `Attribute` module, containing only the deprecated `ex` type.
- Removed deprecated `access` keyword from schema.

## v0.12.3 - 2016-07-11
- Update to latest adpkg.

## v0.12.2 - 2016-06-30

### Fixes
- Fix watermarking and `pkg_datadir` substitution.

## v0.12.1 - 2016-06-29

### Fixes
- Handle deep contexts for add- and delete-selectors.

## v0.12.0 – 2016-06-12

### Added
- `Entity_type.equal`, `Entity_type.required`, `Entity.equal`,
  `Attribute_type.all`, `Entity_type.allowed_mappings`.
- Added `at-list` and `au-list` subcommands.
- Added output detail options for `search`.

### Removed
- Removed various deprecated definitions.

### Fixed
- Column name for transitive views affecting `fts` subcommand.

### Deprecated
- `Entity.type_` with new name `Entity.entity_type`.
