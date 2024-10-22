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
  grouping <- rlang::enquo(.by)

  target_pos <- tidyselect::eval_select(targets, data = .data)

  param_col_names <- make_param_col_names(
    target_pos,
    data = .data,
    norm = .names
  )

  target_names <- param_col_names$target_names
  L_names <- param_col_names$L_names
  S_names <- param_col_names$S_names
  norm_names <- param_col_names$norm_names

  .data <- .data |>
    make_L(
      grouping = !!grouping,
      targets = !!targets,
      fn = \(x) mean(x, na.rm = T)
    ) |>
    make_S(
      grouping = !!grouping,
      targets = !!targets,
      fn = \(x) sd(x, na.rm = T)
    )

  for(targ in names(target_pos)){
    .data <- dplyr::mutate(
      .data,
      !!norm_names[targ] :=
        (!!rlang::sym(targ) - !!rlang::sym(L_names[targ])) / !!rlang::sym(S_names[targ])
    )
  }

  .data <- dplyr::relocate(
    .data,
    tidyselect::all_of(norm_names |> rlang::set_names()),
    .after = dplyr::last(names(target_pos))
  )

  if(!.keep_params){
    .data <- dplyr::select(
      .data,
      -tidyselect::all_of(S_names),
      -tidyselect::all_of(L_names)

    )
  }

  if(.drop_orig){
    .data <- dplyr::select(
      .data,
      -(!!targets)
    )
  }

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

  .data <- dplyr::mutate(
    .data,
    .by = !!grouping,
    L = mean(log(.col), na.rm = T),
    S = 1,
    dplyr::across(
      .col,
      .fns = \(x) log(x) - L,
      .names = .names
    )
  )

  .data <- tidyr::pivot_wider(
    .data,
    names_from = .formant,
    values_from = starts_with(".col"),
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

  if(!.keep_params){
    .data <- dplyr::select(
      .data,
      -c(L,S)
    )
  }

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

  .data <- dplyr::mutate(
    .data,
    .by = !!grouping,
    .formant_num = stringr::str_extract(
      .formant,
      r"{[fF](\d)}",
      group = 1
    ) |> as.numeric(),
    L = 0,
    S = mean(
      .col/(.formant_num - 0.5),
      na.rm = T
    ),
    dplyr::across(
      .col,
      .fns = \(x) x/S,
      .names = .names
    )
  )

  .data <- dplyr::select(
    .data,
    -.formant_num
  )

  .data <- tidyr::pivot_wider(
    .data,
    names_from = .formant,
    values_from = starts_with(".col"),
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

  if(!.keep_params){
    .data <- dplyr::select(
      .data,
      -c(L,S)
    )
  }

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

  .data <- dplyr::mutate(
    .data,
    .corners = dplyr::case_when(
      {{.vowel_col}} %in% .high_front~ "high_front",
      {{.vowel_col}} %in% .low ~ "low",
      .default = NA
    )
  )

  corners <- .data |>
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
