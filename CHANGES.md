# Changelog

## Next release

### Added
- New Boolean option `run.verbose_validation` (CLI arg: `--verbose-validation`) to help with debugging of validator behaviors [#7](https://github.com/ocamlpro/seacoral/pull/7)

### Fixed
- Detection by the validator of buffer overflows on empty arrays [#6](https://github.com/ocamlpro/seacoral/pull/6)
- Unnecessary generation by Klee, of tests that violate pre-conditions (`sc_assume`) [#5](https://github.com/ocamlpro/seacoral/pull/5)
- Handling of NaNs and floating-point literals in test suites [#2](https://github.com/ocamlpro/seacoral/pull/2)


## v0.1.0 ( 2025-10-14 )

* Initial version
