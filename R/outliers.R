safe_mahal <- purrr::safely(stats::mahalanobis, otherwise = NA)


#' Identify Outliers
#' @inheritParams norm_generic
#' @param level Probability level above which tokens are labelled as outliers.
#' @eval options::as_params(".silent" = "tidynorm.silent")
#' @export
identify_outliers <- function(
  .data,
  ...,
  .by = NULL,
  .pre_trans = identity,
  .level = 0.99,
  .call = caller_env(),
  .silent = opt("tidynorm.silent")
) {
  if (env_name(.call) == "global") {
    .call <- current_env()
  }

  # preserve any previous normalization info
  prev_attr <- attributes(.data)$norminfo

  # check if any arguments should have been dotted
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls, .call)

  # capture target columns
  targets <- expr(c(...))

  # check if there was inappropriate grouping,
  # or a missing grouping
  check_by_grouping(.data, {{ .by }}, call = .call)
  check_any_grouping(.data, {{ .by }}, call = .call)

  # grouping <- rlang::enquo(.by)
  group_pos <- tidyselect::eval_select(
    enquo(.by),
    data = .data
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd, arg = "...", call = .call)
  )

  .data <- dplyr::mutate(
    .data,
    .id = dplyr::row_number(),
    .before = 1
  )

  # see if the data is grouped
  grouped_by <- dplyr::group_vars(
    .data
  )

  norm_grouping <- expr({{ .by }})

  .data |>
    dplyr::select(
      any_of(names(target_pos)),
      {{ .by }},
      grouped_by
    ) |>
    tidyr::drop_na() |>
    dplyr::mutate(
      across(
        names(target_pos),
        .pre_trans
      )
    ) |>
    tidyr::nest(
      .by = {{ .by }},
      data = any_of(names(target_pos))
    ) |>
    dplyr::mutate(
      mus = purrr::map(
        !!sym("data"),
        \(x) colMeans(as.matrix(x))
      ),
      Sigma = purrr::map(
        !!sym("data"),
        \(x) stats::cov(as.matrix(x))
      )
    ) |>
    select(
      -!!sym("data")
    ) -> params

  if (length(grouped_by > 0)) {
    .data <- dplyr::group_by(
      .data,
      !!sym(".id"),
      .add = TRUE
    )
  } else {
    norm_grouping <- expr(c(!!norm_grouping, !!sym(".id")))
  }

  .data |>
    tidyr::pivot_longer(
      any_of(names(target_pos))
    ) |>
    mutate(
      value = .pre_trans(!!sym("value"))
    ) |>
    summarise(
      .by = !!norm_grouping,
      data = list(!!sym("value"))
    ) |>
    left_join(
      params,
      by = c(names(group_pos), grouped_by)
    ) |>
    dplyr::mutate(
      .mahal = purrr::pmap(
        list(
          !!sym("data"),
          !!sym("mus"),
          !!sym("Sigma")
        ),
        \(x, m, s) safe_mahal(x, m, s)$result
      )
    ) |>
    select(
      {{ .by }},
      grouped_by,
      !!sym(".id"),
      !!sym(".mahal")
    ) |>
    tidyr::unnest(!!sym(".mahal")) |>
    mutate(
      .prob = stats::pchisq(
        !!sym(".mahal"),
        df = length(target_pos)
      )
    ) -> dists

  .data <- .data |>
    left_join(
      dists,
      by = c(names(group_pos), grouped_by, ".id")
    ) |>
    mutate(
      .outlier = !!sym(".prob") > .level
    )

  if (length(grouped_by) > 0) {
    .data <- .data |>
      ungroup(!!sym(".id"))
  }

  .data <- .data |>
    select(-!!sym(".id"))

  norm_info <- list(
    .op = "Outliers identified along",
    .targets = names(target_pos),
    .pre_trans = as.character(substitute(.pre_trans)),
    .by = c(names(group_pos), grouped_by),
    .level = .level
  )

  attr(.data, "norminfo") <- prev_attr
  .data <- append_norm_info(
    .data,
    norm_info
  )

  wrap_up(.data, .silent)

  .data
}

#' Identify DCT Outliers
#' @inheritParams norm_generic
#' @param level Probability level above which tokens are labelled as outliers.
#' @eval options::as_params(".silent" = "tidynorm.silent")
#' @export
identify_dct_outliers <- function(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .param_col = NULL,
  .level = 0.99,
  .call = caller_env(),
  .silent = opt("tidynorm.silent")
) {
  if (env_name(.call) == "global") {
    .call <- current_env()
  }

  # preserve any previous normalization info
  prev_attr <- attributes(.data)$norminfo

  # check if any arguments should have been dotted
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls, .call)

  # capture target columns
  targets <- expr(c(...))

  # check if there was inappropriate grouping,
  # or a missing grouping
  check_by_grouping(.data, {{ .by }}, call = .call)
  check_any_grouping(.data, {{ .by }}, call = .call)

  # grouping <- rlang::enquo(.by)
  group_pos <- tidyselect::eval_select(
    enquo(.by),
    data = .data
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd, arg = "...", call = .call)
  )

  token_pos <- try_fetch(
    tidyselect::eval_select(enquo(.token_id_col), data = .data),
    error = \(cnd) selection_errors(cnd, arg = "...", call = .call)
  )

  if (length(token_pos) == 0) {
    cli::cli_abort(
      "Must supply {.arg .token_id_col}"
    )
  }

  param_pos <- try_fetch(
    tidyselect::eval_select(enquo(.param_col), data = .data),
    error = \(cnd) selection_errors(cnd, arg = "...", call = .call)
  )

  if (length(param_pos) == 0) {
    cli::cli_abort(
      "Must supply {.arg .param_col}"
    )
  }

  # see if the data is grouped
  grouped_by <- dplyr::group_vars(
    .data
  )

  norm_grouping <- expr({{ .by }})

  .data |>
    dplyr::select(
      any_of(names(target_pos)),
      {{ .by }},
      {{ .param_col }},
      {{ .token_id_col }},
      grouped_by
    ) |>
    tidyr::drop_na() |>
    tidyr::pivot_wider(
      names_from = {{ .param_col }},
      values_from = any_of(names(target_pos))
    ) |>
    tidyr::nest(
      .by = {{ .by }},
      data = starts_with(names(target_pos))
    ) |>
    dplyr::mutate(
      mus = purrr::map(
        !!sym("data"),
        \(x) colMeans(as.matrix(x))
      ),
      Sigma = purrr::map(
        !!sym("data"),
        \(x) stats::cov(as.matrix(x))
      )
    ) |>
    select(
      -!!sym("data")
    ) -> params

  if (length(grouped_by > 0)) {
    .data <- dplyr::group_by(
      .data,
      {{ .token_id_col }},
      .add = TRUE
    )
  } else {
    norm_grouping <- expr(c(!!norm_grouping, {{ .token_id_col }}))
  }

  .data |>
    tidyr::pivot_wider(
      names_from = {{ .param_col }},
      values_from = any_of(names(target_pos))
    ) |>
    tidyr::pivot_longer(
      starts_with(names(target_pos))
    ) |>
    summarise(
      .by = c(!!norm_grouping),
      data = list(!!sym("value"))
    ) |>
    left_join(
      params,
      by = c(names(group_pos), grouped_by)
    ) |>
    dplyr::mutate(
      .mahal = purrr::pmap(
        list(
          !!sym("data"),
          !!sym("mus"),
          !!sym("Sigma")
        ),
        \(x, m, s) safe_mahal(x, m, s, tol = 1e-20)$result
      )
    ) |>
    select(
      {{ .by }},
      grouped_by,
      {{ .token_id_col }},
      !!sym(".mahal")
    ) |>
    tidyr::unnest(!!sym(".mahal")) |>
    mutate(
      .prob = stats::pchisq(
        !!sym(".mahal"),
        df = ncol(params[["Sigma"]][[1]])
      )
    ) -> dists

  .data <- .data |>
    left_join(
      dists,
      by = c(names(group_pos), grouped_by, names(token_pos))
    ) |>
    mutate(
      .outlier = !!sym(".prob") > .level
    )

  norm_info <- list(
    .op = "Outliers identified along",
    .targets = names(target_pos),
    .by = c(names(group_pos), grouped_by),
    .level = .level
  )

  attr(.data, "norminfo") <- prev_attr
  .data <- append_norm_info(
    .data,
    norm_info
  )

  wrap_up(.data, .silent)

  .data
}
