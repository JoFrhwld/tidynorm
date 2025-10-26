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
