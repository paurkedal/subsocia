# Change Log

## [0.12.2] - 2016-06-30

### Fixes

* Fix watermarking and `pkg_datadir` substitution.

## [0.12.1] - 2016-06-29

### Fixes

* Handle deep contexts for add- and delete-selectors.

## [0.12.0] – 2016-06-12

### Added

* `Entity_type.equal`, `Entity_type.required`, `Entity.equal`,
  `Attribute_type.all`, `Entity_type.allowed_mappings`.
* Added `at-list` and `au-list` subcommands.
* Added output detail options for `search`.

### Removed

* Removed various deprecated definitions.

### Fixed

* Column name for transitive views affecting `fts` subcommand.

### Deprecated

* `Entity.type_` with new name `Entity.entity_type`.


[0.12.2]: https://github.com/paurkedal/subsocia/compare/0.12.1...0.12.2
[0.12.1]: https://github.com/paurkedal/subsocia/compare/0.12.0...0.12.1
[0.12.0]: https://github.com/paurkedal/subsocia/compare/0.11...0.12.0
