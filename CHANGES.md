# Change Log

## [Unreleased]

## [0.13.1] - 2016-08-16

### Added
- Added `Soid.compare`.

### Fixed
- Added missing yojson (and implied uri) dependency.
- Fix escape condition and root case in printer, and tweak lexer.

### Removed
- `Subsocia_derived_intf.S.Base` component, indented only for internal use.

## [0.13.0] - 2016-08-14

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

## [0.12.3] - 2016-07-11
- Update to latest adpkg.

## [0.12.2] - 2016-06-30

### Fixes
- Fix watermarking and `pkg_datadir` substitution.

## [0.12.1] - 2016-06-29

### Fixes
- Handle deep contexts for add- and delete-selectors.

## [0.12.0] – 2016-06-12

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


[Unreleased]: https://github.com/paurkedal/subsocia/compare/0.13.1...HEAD
[0.13.1]: https://github.com/paurkedal/subsocia/compare/0.13.0...0.13.1
[0.13.0]: https://github.com/paurkedal/subsocia/compare/0.12.3...0.13.0
[0.12.3]: https://github.com/paurkedal/subsocia/compare/0.12.2...0.12.3
[0.12.2]: https://github.com/paurkedal/subsocia/compare/0.12.1...0.12.2
[0.12.1]: https://github.com/paurkedal/subsocia/compare/0.12.0...0.12.1
[0.12.0]: https://github.com/paurkedal/subsocia/compare/0.11...0.12.0
