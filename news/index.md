# Changelog

## tidynorm (development version)

## tidynorm 0.4.1

CRAN release: 2026-05-10

### Minor bug fixes

- Updates how
  [`checkmate::check_character()`](https://mllg.github.io/checkmate/reference/checkCharacter.html)
  is imported ([\#32](https://github.com/JoFrhwld/tidynorm/issues/32),
  [\#34](https://github.com/JoFrhwld/tidynorm/issues/34))

## tidynorm 0.4.0

CRAN release: 2025-10-26

### New features

- There are now settable options to control the verbosity of tidynorm
  functions, including both informational messages and warnings. See
  [`tidynorm::options()`](https://jofrhwld.github.io/tidynorm/reference/options.md)
  or
  [`tidynorm_options()`](https://jofrhwld.github.io/tidynorm/reference/tidynorm_options.md)
  ([\#26](https://github.com/JoFrhwld/tidynorm/issues/26))

### Minor bug fixes

- Fixed a bug in
  [`reframe_with_dct_smooth()`](https://jofrhwld.github.io/tidynorm/reference/reframe_with_dct_smooth.md)
  which would error when with `.rate = TRUE` or `.accel = TRUE`.
  ([\#23](https://github.com/JoFrhwld/tidynorm/issues/23))

- [`reframe_with_dct_smooth()`](https://jofrhwld.github.io/tidynorm/reference/reframe_with_dct_smooth.md)
  will now return smooths the same length as each original token.
  ([\#25](https://github.com/JoFrhwld/tidynorm/issues/25))

## tidynorm 0.3.1

CRAN release: 2025-10-06

- Patching issue with RcppArmadillo

## tidynorm 0.3.0

CRAN release: 2025-06-16

- Initial CRAN submission.
