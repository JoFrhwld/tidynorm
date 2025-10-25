#' @eval options::as_params()
#' @name options_params
#'
NULL


#' @eval options::as_roxygen_docs()
NULL

options::define_option(
  option = "tidynorm.silent",
  default = FALSE,
  desc = "Suppress normalization information messages when running a norm_*() function.",
  option_name = "tidynorm.silent"
)

options::define_option(
  option = "tidynorm.warnings",
  default = TRUE,
  desc = "Print warnings from tidynorm functions.",
  option_name = "tidynorm.warnings"
)
