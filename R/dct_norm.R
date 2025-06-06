#' Generic Formant DCT Normalization Procedure
#'
#' @inheritParams norm_track_generic
#' @param .data A data frame of formant DCT coefficients
#' @param .param_col A column identifying the DCT parameter number.
#' @details
#' This will normalize vowel formant data that has already had the
#' Discrete Cosine Transform applied (see [dct]) with the following
#' procedure:
#'
#' 1. Location `.L` and Scale `.S` expressions will be used to summarize
#' the zero<sup>th</sup> DCT coefficients.
#' 2. These location and scale will be used to normalize the DCT coefficients.
#'
#' ### Location and Scale expressions
#' [norm_dct_generic] normalizes DCT coefficients directly.
#' If \eqn{F_k} is the k<sup>th</sup> DCT coefficient
#' the normalization procedure is
#'
#' \deqn{
#' \hat{F}_k = \frac{F_k - L'}{\sqrt{2}S}
#' }
#' \deqn{
#' L' = \begin{cases}
#'    L & \text{for }k=0\\
#'    0 & \text{for }k>0
#'  \end{cases}
#' }
#'
#' Rather than requiring users to remember to multiply expressions for \eqn{S}
#' by \eqn{\sqrt{2}}, this is done by [norm_dct_generic] itself, to allow greater
#' parallelism with how [norm_generic] works.
#'
#' The expressions for calculating \eqn{L} and \eqn{S} can be
#' passed to `.L` and `.S`, respectively. Available values for
#' these expressions are
#'
#' \describe{
#'  \item{`.formant`}{The original formant value}
#'  \item{`.formant_num`}{The number of the formant. (e.g. 1 for F1, 2 for F2 etc)}
#' }
#'
#' Along with any data columns from your original data.
#'
#' ### Identifying tokens
#' DCT normalization only works on a by-token basis, so there must be a column that
#' uniquely identifies (or, in combination with a `.by` grouping, uniquely
#' identifies) each individual token. This column should be passed to
#' `.token_id_col`.
#'
#' @returns
#' A data frame of normalized DCT coefficients.
#'
#' @example inst/examples/ex-norm_dct_generic.R
#'
#' @export
norm_dct_generic <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .param_col = NULL,
    .L = 0,
    .S = 1 / sqrt(2),
    .by_formant = FALSE,
    .by_token = FALSE,
    .names = "{.formant}_n",
    .silent = FALSE,
    .drop_orig = FALSE,
    .call = caller_env()) {
  if (env_name(.call) == "global") {
    .call2 <- current_env()
  }
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls, .call2)

  .names <- glue::glue(.names, .formant = ".formant")

  targets <- expr(c(...))
  cols <- enquos(
    .by = .by,
    .token_id_col = .token_id_col,
    .param_col = .param_col
  )

  for (x in cols) {
    try_fetch(
      tidyselect::eval_select(x, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  check_grouping(.data, {{ .by }}, .call)

  grouping_list <- make_dct_grouping(
    .data,
    {{ .by }},
    {{ .token_id_col }}
  )
  .data <- grouping_list$.data
  by_grouping <- grouping_list$by_grouping
  joining <- grouping_list$joining

  .dct_data <- .data |>
    tidyr::pivot_longer(
      cols = !!targets,
      names_to = ".formant_name",
      values_to = ".formant"
    ) |>
    dplyr::mutate(
      .formant_num = stringr::str_extract(
        !!sym(".formant_name"),
        r"{\d}"
      ) |>
        as.numeric()
    )

  grouped_by <- length(dplyr::group_vars(.dct_data)) > 0

  # augment grouping as necessary to
  # match .by_formant
  # byformant <- NULL
  if (.by_formant & grouped_by) {
    .dct_data <- dplyr::group_by(
      .dct_data,
      !!sym(".formant_name"),
      .add = TRUE
    )
    by_grouping2 <- expr(NULL)
    by_grouping_noid <- expr(NULL)
  } else if (.by_token & grouped_by) {
    .dct_data <- dplyr::group_by(
      .dct_data,
      {{ .token_id_col }},
      .add = TRUE
    )
    by_grouping2 <- expr(NULL)
    by_grouping_noid <- expr(NULL)
  } else if (grouped_by) {
    by_grouping2 <- expr(NULL)
    by_grouping_noid <- expr(NULL)
  } else if (.by_formant) {
    by_grouping2 <- expr(c(!!by_grouping, sym(".formant_name")))
    by_grouping_noid <- expr(c({{ .by }}, sym(".formant_name")))
  } else if (.by_token) {
    by_grouping2 <- expr(c(!!by_grouping, {{ .token_id_col }}))
    by_grouping_noid <- expr(c({{ .by }}, {{ .token_id_col }}))
  } else {
    by_grouping2 <- by_grouping
    by_grouping_noid <- expr({{ .by }})
  }

  zeroth <- dplyr::filter(
    .dct_data,
    .by = !!by_grouping2,
    {{ .param_col }} == min({{ .param_col }})
  )

  if (grouped_by & !.by_token) {
    zeroth <- zeroth |>
      dplyr::ungroup({{ .token_id_col }})
  }

  if (grouped_by) {
    keepit <- "keep"
  } else {
    keepit <- NULL
  }

  norm_params <- zeroth |>
    dplyr::summarise(
      .by = !!by_grouping_noid,
      .L = {{ .L }},
      .S = {{ .S }} * sqrt(2),
      .groups = keepit
    )

  param_joining <- c(
    dplyr::group_vars(norm_params),
    names(
      tidyselect::eval_select(by_grouping_noid, norm_params)
    )
  )

  .dct_with_norm <- dplyr::left_join(
    .dct_data,
    norm_params,
    by = param_joining
  )

  normed <- .dct_with_norm |>
    dplyr::mutate(
      "{.names}" := case_when(
        {{ .param_col }} == 0 ~ (!!sym(".formant") - !!sym(".L")) / !!sym(".S"),
        .default = !!sym(".formant") / !!sym(".S")
      )
    ) |>
    dplyr::select(
      -c(
        !!sym(".L"),
        !!sym(".S"),
        !!sym(".formant_num")
      )
    )

  if (.drop_orig) {
    normed <- dplyr::select(
      normed,
      -!!sym(".formant")
    )
  }

  normed <- normed |>
    tidyr::pivot_wider(
      names_from = !!sym(".formant_name"),
      values_from = c(starts_with(".formant") & !any_of(".formant_name")),
      names_glue = "{.formant_name}_{.value}"
    )

  call_tree <- trace_back()$call |>
    purrr::map(as_label) |>
    unlist()

  if (!"norm_track_generic(...)" %in% call_tree) {
    normed <- normed |>
      dplyr::rename_with(
        .fn = \(x) stringr::str_remove(x, "_.formant")
      )
  }

  return(normed)
}
