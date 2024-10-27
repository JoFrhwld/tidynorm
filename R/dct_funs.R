#' lobanov_dct_norm_fun
#' @export
#' @keywords internal
lobanov_dct_norm_fun <- function(
    .data,
    .by,
    .token_id_col,
    .by_formant,
    .names,
    .param_col
){

  group_pos <- tidyselect::eval_select(enquo(.by), .data)

  joining <- c(names(group_pos), as_name(enquo(.param_col)))
  if(.by_formant){
    base_by <- expr(c({{.by}}, !!sym(".formant")))
    joining <- c(joining, ".formant")
  } else {
    base_by <- expr(c({{.by}}))
  }


  zeroth <- dplyr::filter(
    .data,
    .by = c({{.by}}, {{.token_id_col}}, !!sym(".formant")),
    {{.param_col}} == min({{.param_col}})
  )


  norm_params <- dplyr::summarise(
    zeroth,
    .by = c(!!base_by, {{.param_col}}),
    L = base::mean(.col, na.rm = T),
    S = stats::sd(.col, na.rm = T) * sqrt(2)
  )

  .data <- dplyr::left_join(
    .data,
    norm_params,
    by = joining
  ) |>
    tidyr::replace_na(
      list(
        L = 0
      )
    ) |>
    dplyr::mutate(
      .by = !!base_by,
      S = min(!!sym("S"), na.rm = T)
    )

  .data <- .data |>
    mutate(
      across(
        .col,
        .fns = \(x) (x - !!sym("L")) / !!sym("S"),
        .names = .names
      )
    ) |>
    dplyr::select(
      -!!sym(".col")
    )

  return(.data)
}
