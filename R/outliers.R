#' Identify Outliers
#' @inheritParams norm_generic
#' @eval options::as_params(".silent" = "tidynorm.silent")
#' @export
identify_outliers <- function(
    .data,
    ...,
    .by = NULL,
    .pre_trans = \(x)x,
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
    .id = dplyr::row_number()
  ) |>
    dplyr::relocate(
      !!sym(".id"),
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
    ) ->
    params

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
    pivot_longer(
      any_of(names(target_pos))
    ) |>
    mutate(
      value = .pre_trans(!!sym("value"))
    ) |>
    summarise(
      .by = !!norm_grouping,
      data = list(!!sym("value"))
    )  |>
    left_join(
      params,
      by = c(names(group_pos), grouped_by)
    ) |>
    dplyr::mutate(
      mahal = pmap(
        list(
          !!sym("data"),
          !!sym("mus"),
          !!sym("Sigma")
        ),
        \(x, m, s) mahalanobis(x, m, s)
      )
    ) |>
    select(
      {{.by}},
      grouped_by,
      !!sym(".id"),
      !!sym("mahal")
    ) |>
    unnest(!!sym("mahal")) |>
    mutate(
      prob = pchisq(
        !!sym("mahal"),
        df = length(target_pos)
      )
    ) ->
    dists

  .data <- .data |>
    left_join(
      dists,
      by = c(names(group_pos), grouped_by, ".id")
    ) |>
    mutate(
      outlier = !!sym("prob") > .level
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
