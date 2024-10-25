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
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_n",
    .silent = FALSE,
    .call = caller_env()
){
  prev_attr <- attributes(.data)$norminfo

  targets <- expr(c(...))
  tokens <- enquo(.token_id_col)
  time <- enquo(.time_col)


  time_pos <- try_fetch(
    tidyselect::eval_select(time, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  ## a token column is required
  tokens_pos <- try_fetch(
    tidyselect::eval_select(tokens, data = .data),
    error = \(cnd) selection_errors(cnd)
  )


  check_grouping(.data, enquo(.by), .call)
  grouping <- rlang::enquo(.by)
  group_pos <- tidyselect::eval_select(
    grouping, data = .data
  )

  if(length(time_pos) == 1){
    .time_data <- dplyr::arrange(.data, !!time) |>
      dplyr::select(
        {{.by}},
        {{.token_id_col}},
        {{.time_col}}
      ) |>
      dplyr::mutate(
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
      !!targets,
      names_to = ".formant",
      values_to = ".col"
    )

  grouped_by <- dplyr::group_vars(
    .dct_data
  )

  # augment grouping as necessary to
  # match .by_formant
  if(.by_formant & length(grouped_by > 0)){
    .dct_data <- dplyr::group_by(
      .dct_data,
      !!sym(".formant"),
      .add = TRUE
    )
    norm_group <- grouping
  } else if(.by_formant){
    norm_group <- rlang::expr(
      c(!!grouping, !!sym(".formant"))
    )
  } else {
    norm_group <- grouping
  }


  normed<- .norm_fun(
    .data = .dct_data,
    grouping = norm_group,
    .names = .names,
    .param_col = .param
  )

  norm_data <- normed |>
    tidyr::pivot_wider(
      names_from = .formant,
      values_from = .col,
      names_glue = .names
    )
  return(norm_data)
}
