# tidynorm (development version)

# tidynorm 0.5.0

## New Features

* You can now pre-aggregate data by vowel category before calculating the 
  normalization parameters with `.agg_by` and `.agg_fun` arguments to all
  vowel-extrinsic techniques (#43)

# tidynorm 0.4.1

## Minor bug fixes

* Updates how `checkmate::check_character()` is imported (#32, #34)

# tidynorm 0.4.0

## New features
  
* There are now settable options to control the verbosity of 
  tidynorm functions, including both informational messages
  and warnings. See `tidynorm::options()` or `tidynorm_options()` (#26)
  
## Minor bug fixes
* Fixed a bug in `reframe_with_dct_smooth()` which would error when 
  with `.rate = TRUE` or `.accel = TRUE`. (#23)

* `reframe_with_dct_smooth()` will now return smooths the same length 
  as each original token. (#25)

# tidynorm 0.3.1

* Patching issue with RcppArmadillo


# tidynorm 0.3.0

* Initial CRAN submission.
