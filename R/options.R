#' @eval options::as_params()
#' @name options_params
#'
NULL

#' @eval options::as_roxygen_docs(
#'   desc = stringr::str_c(
#'     sep = " ",
#'     "Options to control the verbosity of tidynorm functions.",
#'     "The convenience function [tidynorm_options()] will set these",
#'     "options within your current session. For a common behavior across",
#'     "R sessions, set the environment variables as described below.",
#'     "If you've silenced informational messages and want to double check",
#'     "what normalization steps have been taken, use [check_norm()]."
#'   )
#' )
NULL

options::define_option(
  option = "tidynorm.silent",
  default = FALSE,
  desc = "Suppress normalization information messages when running a `norm_*()` function.",
  option_name = "tidynorm.silent"
)

options::define_option(
  option = "tidynorm.warnings",
  default = TRUE,
  desc = "Print warnings from tidynorm functions.",
  option_name = "tidynorm.warnings"
)


#' Set tidynorm options
#'
#' Set tidynorm verbosity
#'
#' @eval options::as_params(".silent" = "tidynorm.silent")
#' @eval options::as_params(".warnings" = "tidynorm.warnings")
#' @importFrom options opt
#'
#' @seealso [tidynorm::options]
#'
#' @export
#'
#' @examples
#' tidynorm_options(.silent = TRUE, .warnings = FALSE)
#'
#' speaker_data |>
#'   norm_generic(F1:F3) ->
#'   norm1
#'
#' tidynorm_options(.silent = FALSE, .warnings = TRUE)
#'
#' speaker_data |>
#'   norm_generic(F1:F3) ->
#'   norm2
#'
tidynorm_options <- function(
    .silent = opt("tidynorm.silent"),
    .warnings = opt("tidynorm.warnings")
){
  options::opt_set("tidynorm.silent", .silent)
  options::opt_set("tidynorm.warnings", .warnings)
}
