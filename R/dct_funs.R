#' lobanov_dct_norm_fun
#' @export
#' @keywords internal
lobanov_dct_norm_fun <- function(
    .data,
    grouping,
    .names,
    .param_col
){

  zeroth <- dplyr::filter(
    .data,
    {{.param_col}} == min({{.param_col}})
  )

  norm_params <- dplyr::summarise(
      zeroth,
      .by = c(!!grouping),
      L = mean(.col, na.rm = T),
      S = sd(.col, na.rm = T) * sqrt(2)
    )

  # .data <- dplyr::left_join(
  #   .data,
  #   norm_params
  # ) |>
  #   dplyr::mutate(
  #     across(
  #       .col,
  #       .fns =
  #       {{.param_col}} == min({{.param_col}}) ~ (.col - L)/S,
  #       .default = .col/S
  #     )
  #   )

  return(.data)
}
