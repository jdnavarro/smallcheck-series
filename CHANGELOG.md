# Change Log
All notable changes to this project will be documented in this file. This file
follows the formatting recommendations from [Keep a
CHANGELOG](http://keepachangelog.com/). This project adheres to [Semantic
Versioning](http://semver.org/).

## [0.3] - 2015-5-25
### Added
- Serial instance for `Map`.
- `zipLogic` for *zipping* instances. Thanks to Roman Cheplyaka
  [@feuerbach](https://github.com/feuerbach).

### Fixed
- Compatibility with GHC < 7.10.

## [0.2] - 2015-4-28
### Changed
- General renaming to, hopefully, make functions more clear.
### Fixed
- Series don't repeat elements anymore. Kudos to Roman Cheplyaka for
  reporting this issue.

## [0.1] - 2015-4-27
### Added
- Initial set of utilities for creating `ByteString` and `Text` `Series`.
- `Serial` `ByteString` and `Text` instances.

[0.3]: https://github.com/jdnavarro/smallcheck-series/compare/v0.2...v0.3
[0.2]: https://github.com/jdnavarro/smallcheck-series/compare/v0.1...v0.2
[0.1]: https://github.com/jdnavarro/smallcheck-series/compare/49b5b0...v0.1
