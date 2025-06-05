#' check norm
#' @noRd
check_norm <- function(data) {
  if (!is.null(attr(data, "normalized"))) {
    cli_inform(
      c(
        "*" = "Normalized"
      )
    )
  } else {
    cli_inform(
      c(
        "x" = "Not normalized with a {.pkg tidynorm} procedure."
      )
    )
  }
}

norminfo_to_message <- function(data) {
  norminfo <- attributes(data)$norminfo
}
