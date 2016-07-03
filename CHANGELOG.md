# Change Log
All notable changes to this project will be documented in this file. This file
follows the formatting recommendations from [Keep a
CHANGELOG](http://keepachangelog.com/). This project adheres to [Semantic
Versioning](http://semver.org/).

## [0.6] - 2016-07-03
### Added
- Support for `base-4.9` which comes bundled with `GHC-8.0.1`.

### Removed
- Support for `GHC-7.8.4`. For some reason with this version the `transformers`
  dependency is pinned to 0.3.0.0. I don't have time to fix this issue but if
  you know how to fix it PRs are welcome.
- Support for stack.

## [0.5.1] - 2015-09-01
### Fixed
- Intances of `Word` and `Int` now stop generating at its maxium bound.

## [0.5] - 2015-08-31
### Changed
- `Text` and `ByteString` Serial instances are now exhaustive.

### Added
- `Serial` and `CoSerial` instances for `Word`, `Word8`, `Int16`.
- `Serial` and `CoSerial` instances for `Int8`, `Int16`.

## [0.4] - 2015-08-06
### Added
- Support for stack.
- `CoSerial` instances.

## [0.3] - 2015-05-25
### Added
- Serial instance for `Map`.
- `zipLogic` for *zipping* instances. Thanks to Roman Cheplyaka
  [@feuerbach](https://github.com/feuerbach).

### Fixed
- Compatibility with GHC < 7.10.

## [0.2] - 2015-04-28
### Changed
- General renaming to, hopefully, make functions more clear.
### Fixed
- Series don't repeat elements anymore. Kudos to Roman Cheplyaka for
  reporting this issue.

## [0.1] - 2015-04-27
### Added
- Initial set of utilities for creating `ByteString` and `Text` `Series`.
- `Serial` `ByteString` and `Text` instances.

[0.6]: https://github.com/jdnavarro/smallcheck-series/compare/v0.5.1...v0.6
[0.5.1]: https://github.com/jdnavarro/smallcheck-series/compare/v0.5...v0.5.1
[0.5]: https://github.com/jdnavarro/smallcheck-series/compare/v0.4...v0.5
[0.4]: https://github.com/jdnavarro/smallcheck-series/compare/v0.3...v0.4
[0.3]: https://github.com/jdnavarro/smallcheck-series/compare/v0.2...v0.3
[0.2]: https://github.com/jdnavarro/smallcheck-series/compare/v0.1...v0.2
[0.1]: https://github.com/jdnavarro/smallcheck-series/compare/49b5b0...v0.1
