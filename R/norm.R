#' norm generic
#' @export
norm_generic <- function(
  .data,
  ...,
  .by = NULL,
  .norm_fun = identity_norm_fun,
  .by_formant = FALSE,
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.col}_n"
){
  targets <- rlang::expr(c(...))
  grouping <- rlang::enquo(.by)

  target_pos <- tidyselect::eval_select(targets, data = .data)

  targets <- rlang::expr(c(...))
  grouping <- rlang::enquo(.by)

  target_pos <- tidyselect::eval_select(targets, data = .data)

  .data <- dplyr::mutate(
    .data,
    .id = dplyr::row_number()
  )

  .data <- tidyr::pivot_longer(
    .data,
    !!targets,
    names_to = ".formant",
    values_to = ".col"
  )

  if(.by_formant){
    norm_group <- rlang::expr(
      c(!!grouping, .formant)
    )
  } else {
    norm_group <- grouping
  }

  .data <- .norm_fun(
    .data,
    norm_group,
    .names
  )

  if(.keep_params){
    values_target <- rlang::expr(c(
      tidyselect::starts_with(".col"),
      tidyselect::all_of(c("L", "S"))
    ))
  } else {
    values_target <- rlang::expr(
      tidyselect::starts_with(".col")
    )
    .data <- dplyr::select(
      .data,
      -c(L, S)
    )
  }

  .data <- tidyr::pivot_wider(
    .data,
    names_from = .formant,
    values_from = !!values_target,
    names_glue = "{.formant}_{.value}"
  )

  .data <- dplyr::relocate(
    .data,
    tidyselect::matches("_.col"),
    .after = target_pos[1]-1
  )

  .data <- dplyr::rename_with(
    .data,
    .cols = tidyselect::matches("_.col"),
    .fn = \(x) stringr::str_remove(x, "_.col")
  )

  if(.drop_orig){
    .data <- dplyr::select(
      .data,
      -(!!targets)
    )
  }

  return(.data)

}

#' Lobanov Normalize
#' @importFrom rlang `:=`
#' @export
norm_lobanov <- function(
    .data,
    ...,
    .by = NULL,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_z"
){

  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .norm_fun = lobanov_norm_fun,
    .by_formant = TRUE,
    .drop_orig = .drop_orig,
    .keep_params = .keep_params,
    .names = .names
  )

  return(.data)

}

#' Nearey Normalization
#' @importFrom rlang `!!`
#' @export
norm_nearey <- function(
    .data,
    ...,
    .by = NULL,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_lm"
){
  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .norm_fun = nearey_norm_fun,
    .by_formant = FALSE,
    .drop_orig = .drop_orig,
    .keep_params = .keep_params,
    .names = .names
  )

  return(.data)

}

#' Delta F Method
#' @export
norm_deltaF <- function(
    .data,
    ...,
    .by = NULL,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_df"
){
  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .norm_fun = deltaF_norm_fun,
    .by_formant = FALSE,
    .drop_orig = .drop_orig,
    .keep_params = .keep_params,
    .names = .names
  )

  return(.data)

}

#' Watt & Fabricius method
#' @export
norm_wattfab <-  function(
    .data,
    ...,
    .vowel_col,
    .high_front = NULL,
    .low = NULL,
    .by = NULL,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_wf"
){
  targets <- rlang::expr(c(...))
  grouping <- rlang::enquo(.by)

  target_pos <- tidyselect::eval_select(targets, data = .data)

  param_col_names <- make_param_col_names(
    target_pos,
    data = .data,
    norm = .names
  )

  target_names <- param_col_names$target_names
  norm_names <- param_col_names$norm_names

  .data_c <- dplyr::mutate(
    .data,
    .corners = dplyr::case_when(
      {{.vowel_col}} %in% .high_front~ "high_front",
      {{.vowel_col}} %in% .low ~ "low",
      .default = NA
    )
  )

  corners <- .data_c |>
    dplyr::filter(
      .corners %in% c("high_front", "low")
    ) |>
    tidyr::pivot_longer(
      !!targets,
      names_to = ".formant",
      values_to = ".col"
    ) |>
    dplyr::summarise(
      .by = c(!!grouping, .formant, .corners),
      .col = mean(.col, na.rm = TRUE)
    ) |>
    tidyr::pivot_wider(
      names_from = c(.formant, .corners),
      values_from = .col
    ) |>
    dplyr::rename_with(
      .fn = tolower
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      f1_high_back = f1_high_front,
      f2_high_back = f2_high_front,
      f2_low = median(c(f2_high_front, f2_high_back))
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      .by = !!grouping,
      f1_L = 0,
      f2_L = 0,
      f1_S = (f1_high_front + f1_high_back + f1_low)/3,
      f2_S = (f2_high_front + f2_high_back + f2_low)/3
    ) |>
    dplyr::select(
      -tidyselect::matches("high"),
      -tidyselect::matches("low")
    )

  if(length(tidyselect::eval_select(grouping, .data)) > 0){
    .data <- dplyr::left_join(
      .data,
      corners,
      by = dplyr::join_by({{grouping}})
    )
  } else {
    .data <- dplyr::cross_join(
      .data,
      corners
    )
  }

  .data <- dplyr::mutate(
    .data,
    !!rlang::sym(norm_names[1]) :=
      !!rlang::sym(target_names[1]) / f1_S,

    !!rlang::sym(norm_names[2]) :=
      !!rlang::sym(target_names[2]) / f2_S
  )

  .data <- dplyr::relocate(
    .data,
    tidyselect::all_of(norm_names[1:2] |> rlang::set_names()),
    .after = target_pos[2]
  )

  return(.data)

}
