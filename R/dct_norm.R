#' Normalize Formant Track
#' @export
norm_track_generic <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .norm_fun = lobanov_dct_norm_fun,
    .by_formant = FALSE,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_n",
    .silent = FALSE,
    .call = caller_env()
){
  prev_attr <- attributes(.data)$norminfo

  targets <- expr(c(...))
  cols <- enquos(
    .by = .by,
    .token_id_col = .token_id_col,
    .time_col = .time_col
  )
  grouping <- expr(c({{.by}}, {{.token_id_col}}))

  for(x in cols){
    try_fetch(
      tidyselect::eval_select(x, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  group_pos <- tidyselect::eval_select(enquo(.by), .data)
  check_grouping(.data, cols$.by, .call)

  if(!quo_is_null(cols$.time_col)){
    .time_data <- dplyr::arrange(.data, {{.time_col}}) |>
      dplyr::select(
        {{.by}},
        {{.token_id_col}},
        {{.time_col}}
      ) |>
      dplyr::mutate(
        .by = c({{.by}}, {{.token_id_col}}),
        .row = dplyr::row_number()
      )

    .data <- dplyr::mutate(
      .data,
      .by = c({{.by}}, {{.token_id_col}}),
      .row = dplyr::row_number()
    )
  }

  .dct_data <- reframe_as_dct(
    .data,
    !!targets,
    .token_id_col = {{.token_id_col}},
    .by = {{.by}},
    .time_col = {{.time_col}},
    .order = .order
  ) |>
    tidyr::pivot_longer(
      cols = !!targets,
      names_to = ".formant",
      values_to = ".col"
    )

  grouped_by <- dplyr::group_vars(
    .dct_data
  )

  # augment grouping as necessary to
  # match .by_formant
  #byformant <- NULL
  if(.by_formant & length(grouped_by > 0)){
    .dct_data <- dplyr::group_by(
      .dct_data,
      !!sym(".formant"),
      .add = TRUE
    )
  }

  normed<- .norm_fun(
    .data = .dct_data,
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .by_formant = .by_formant,
    .names = .names,
    .param_col = .param
  )

  if(!.keep_params){
    normed <- dplyr::select(
      normed,
      -sym("L"), -sym("S")
    )
  }

  normed <- normed |>
    tidyr::pivot_wider(
      names_from = !!sym(".formant"),
      values_from = c(starts_with(".col"), any_of(c("L", "S"))),
      names_glue = "{.formant}_{.value}"
    )

  if(.return_dct){
    normed <- normed |>
      dplyr::rename_with(
        .fn = \(x) stringr::str_remove(x, "_.col")
      )
    return(normed)
  }

  normed_track <- reframe_with_idct(
    normed,
    matches(".col_"),
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .param_col = !!sym(".param"),
    .n = !!sym(".n")
  )

  if(!quo_is_null(cols$.time_col)){
    normed_track <- normed_track |>
      dplyr::select(
        -sym(".time")
      ) |>
      left_join(
        .time_data,
        by = c(names(group_pos), as_name(enquo(.token_id_col)), ".row")
      )
  }

  normed_track <- normed_track |>
    dplyr::rename_with(
      .fn = \(x) stringr::str_remove(x, "_.col")
    )

  return(normed_track)
}
