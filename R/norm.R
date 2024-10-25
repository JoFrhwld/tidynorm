#' Generic normalization procedure
#'
#' This is a generic normalization procedure with which
#' you can create your own normalization method.
#'
#' @param .data A data frame containing vowel formant data
#' @param ... [`<tidy-select>`][dplyr::dplyr_tidy_select] One or more unquoted
#' expressions separated by commas. These should target the vowel formant
#' data columns.
#' @param .by  [`<tidy-select>`][dplyr::dplyr_tidy_select] A selection of
#' columns to group by. Typically a column of speaker IDs.
#' @param .norm_fun A function to apply to a pivoted long data frame. See
#' Details for how to write such a function.
#' @param .by_formant Whether or not the normalization method is formant
#' intrinsic.
#' @param .drop_orig Whether or not to drop the original formant data columns.
#' @param .keep_params Whether or not to keep the Location (`*_L`) and scale
#' (`*_S`) normalization parameters
#' @param .names A [glue::glue()] expression for naming the normalized
#' data columns. The `"{.col}"` portion corresponds to the name of the original
#' formant columns.
#' @param .silent Whether or not the informational message should be printed.
#' @param .call Used for internal purposes.
#'
#' @details
#' The following `norm_*` procedures are built on top of `norm_generic()`.
#'
#' - [norm_lobanov]
#' - [norm_nearey]
#' - [norm_deltaF]
#'
#' You can define a custom normalization function to pass
#' to `.norm_fun` that takes the following arguments
#'
#' - `.data`
#' - `.grouping`
#' - `.names`
#'
#' The `.data` data frame will be your original data frame with the
#' formant columns pivoted longer. The formant names will be available
#' in a column called `.formant`, and the formant values in a column
#' called `.col`. Any Location parameters should be assigned to a
#' column called `L`, and any Scale parameters should be assigned
#' to a column called `S`. A median-based normalization function might
#' look like the following.
#'
#' ```r
#' median_norm_fun <- function(.data, .grouping, .names){
#'   .data |>
#'     mutate(
#'       .by = !!.grouping,
#'       L = median(.col),
#'       S = mad(.col),
#'       across(
#'         .col,
#'         .fns = \(x) (x - L)/S,
#'         .names = .names
#'       )
#'     )
#'  return(.data)
#' }
#' ```
#'
#' If the method is meant to be formant ex/intrinsic, as
#' defined by `.by_formant`, this will be accounted for by
#' the grouping variables in the `.grouping` argument passed
#' to the normalization function.
#'
#' @example inst/examples/ex-norm_generic.R
#'
#' @export
norm_generic <- function(
  .data,
  ...,
  .by = NULL,
  .norm_fun = identity_norm_fun,
  .by_formant = FALSE,
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.col}_n",
  .silent = FALSE,
  .call = caller_env()
){

  prev_attr <- attributes(.data)$norminfo

  targets <- rlang::expr(c(...))

  check_grouping(.data, enquo(.by), .call)
  grouping <- rlang::enquo(.by)
  group_pos <- tidyselect::eval_select(
    grouping, data = .data
  )

  # evaluating for the number of
  # targeted columns
  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd, call = .call)
  )
  check_n_target(target_pos, n = 2, call = .call)

  # adding an id col for safety in
  # pivoting
  .data <- dplyr::mutate(
    .data,
    .id = dplyr::row_number()
  ) |>
    dplyr::relocate(
      !!sym(".id"),
      .before = 1
    )

  # all normalization happens
  # longwise
  .data <- tidyr::pivot_longer(
    .data,
    !!targets,
    names_to = ".formant",
    values_to = ".col"
  )

  # see if the data is grouped
  grouped_by <- dplyr::group_vars(
    .data
  )

  # augment grouping as necessary to
  # match .by_formant
  if(.by_formant & length(grouped_by > 0)){
    .data <- dplyr::group_by(
      .data,
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

  # apply provided norm_fun
  .data <- .norm_fun(
    .data,
    norm_group,
    .names
  )


  # set up value columns for pivoting
  # back wide
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
      -c(!!sym("L"), !!sym("S"))
    )
  }

  # pivot_back wide
  .data <- tidyr::pivot_wider(
    .data,
    names_from = !!sym(".formant"),
    values_from = !!values_target,
    names_glue = "{.formant}_{.value}"
  )

  # move normalized columns adjacent to
  # original
  .data <- dplyr::relocate(
    .data,
    tidyselect::matches("_.col"),
    .after = target_pos[length(target_pos)]
  )

  # remove _.col from names
  .data <- dplyr::rename_with(
    .data,
    .cols = tidyselect::matches("_.col"),
    .fn = \(x) stringr::str_remove(x, "_.col")
  )

  # if .drop_orig
  if(.drop_orig){
    .data <- dplyr::select(
      .data,
      -(!!targets)
    )
  }

  attr(.data, "normalized") <- TRUE
  attr(.data, "norminfo") <- c(
    prev_attr,
    list(
      list(
        .by_col = .by_formant,
        .targets = names(target_pos),
        .norm_cols = glue::glue(.names, .col = names(target_pos)),
        .by = names(group_pos)
      )
    )
  )

  if(!.silent){
    wrap_up(
      .data,
      target_pos,
      enquo(.by),
      .by_formant,
      .names
    )
  }


  return(.data)

}

#' Lobanov Normalize
#' @inheritParams norm_generic
#'
#' @param .by_formant Ignored by this procedure
#'
#' @details
#'
#' \deqn{
#'   \hat{F_{ij}} = \frac{F_{ij} - L_i}{S_i}
#' }
#'
#' \deqn{
#'   L_i = \frac{1}{N}\sum_{j=1}^{N}F_{ij}
#' }
#'
#' \deqn{
#'   S_i = \sqrt{\frac{\sum(F_{ij}-L_i)^2}{N-1}}
#' }
#'
#' @references
#' Lobanov, B. (1971). Classification of Russian vowels spoken by different listeners.
#' Journal of the Acoustical Society of America, 49, 606–608.
#'
#'
#' @export
norm_lobanov <- function(
    .data,
    ...,
    .by = NULL,
    .by_formant = TRUE,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_z",
    .silent = FALSE
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
    .names = .names,
    .silent = .silent
  )

  norminfo <- attr(.data, "norminfo")
  norminfo[[length(norminfo)]]$norm_procedure <- "norm_lobanov"
  attr(.data, "norminfo") <- norminfo

  return(.data)

}

#' Nearey Normalization
#' @inheritParams norm_generic
#' @importFrom rlang `!!`
#'
#' @references
#' Nearey, T. M. (1978). Phonetic Feature Systems for Vowels \[Ph.D.\].
#' University of Alberta.
#' @export
norm_nearey <- function(
    .data,
    ...,
    .by = NULL,
    .by_formant = FALSE,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_lm",
    .silent = FALSE
){
  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .norm_fun = nearey_norm_fun,
    .by_formant = .by_formant,
    .drop_orig = .drop_orig,
    .keep_params = .keep_params,
    .names = .names,
    .silent = .silent
  )

  norminfo <- attr(.data, "norminfo")
  norminfo[[length(norminfo)]]$norm_procedure <- "norm_nearey"
  attr(.data, "norminfo") <- norminfo

  return(.data)

}

#' Delta F Method
#' @inheritParams norm_generic
#'
#' @param .by_formant Ignored by this procedure
#' @references
#' Johnson, K. (2020). The ΔF method of vocal tract length normalization for vowels.
#' Laboratory Phonology: Journal of the Association for Laboratory Phonology, 11(1),
#' Article 1. [https://doi.org/10.5334/labphon.196]
#'
#' @example inst/examples/ex-norm_deltaF.R
#'
#' @export
norm_deltaF <- function(
    .data,
    ...,
    .by = NULL,
    .by_formant = FALSE,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.col}_df",
    .silent = FALSE
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
    .names = .names,
    .silent = .silent
  )

  norminfo <- attr(.data, "norminfo")
  norminfo[[length(norminfo)]]$norm_procedure <- "norm_deltaF"
  attr(.data, "norminfo") <- norminfo

  return(.data)

}

#' #' Watt & Fabricius method
#' #' @inheritParams norm_generic
#' #' @export
#' norm_wattfab <-  function(
#'     .data,
#'     ...,
#'     .vowel_col,
#'     .high_front = NULL,
#'     .low = NULL,
#'     .by = NULL,
#'     .by_formant = TRUE,
#'     .drop_orig = FALSE,
#'     .keep_params = FALSE,
#'     .names = "{.col}_wf",
#'     .silent = FALSE
#' ){
#'   targets <- rlang::expr(c(...))
#'   grouping <- rlang::enquo(.by)
#'
#'   target_pos <- tidyselect::eval_select(targets, data = .data)
#'
#'   param_col_names <- make_param_col_names(
#'     target_pos,
#'     data = .data,
#'     norm = .names
#'   )
#'
#'   target_names <- param_col_names$target_names
#'   norm_names <- param_col_names$norm_names
#'
#'   .data_c <- dplyr::mutate(
#'     .data,
#'     .corners = dplyr::case_when(
#'       {{.vowel_col}} %in% .high_front~ "high_front",
#'       {{.vowel_col}} %in% .low ~ "low",
#'       .default = NA
#'     )
#'   )
#'
#'   corners <- .data_c |>
#'     dplyr::filter(
#'       .corners %in% c("high_front", "low")
#'     ) |>
#'     tidyr::pivot_longer(
#'       !!targets,
#'       names_to = ".formant",
#'       values_to = ".col"
#'     ) |>
#'     dplyr::summarise(
#'       .by = c(!!grouping, .formant, .corners),
#'       .col = mean(.col, na.rm = TRUE)
#'     ) |>
#'     tidyr::pivot_wider(
#'       names_from = c(.formant, .corners),
#'       values_from = .col
#'     ) |>
#'     dplyr::rename_with(
#'       .fn = tolower
#'     ) |>
#'     dplyr::rowwise() |>
#'     dplyr::mutate(
#'       f1_high_back = f1_high_front,
#'       f2_high_back = f2_high_front,
#'       f2_low = median(c(f2_high_front, f2_high_back))
#'     ) |>
#'     dplyr::ungroup() |>
#'     dplyr::mutate(
#'       .by = !!grouping,
#'       f1_L = 0,
#'       f2_L = 0,
#'       f1_S = (f1_high_front + f1_high_back + f1_low)/3,
#'       f2_S = (f2_high_front + f2_high_back + f2_low)/3
#'     ) |>
#'     dplyr::select(
#'       -tidyselect::matches("high"),
#'       -tidyselect::matches("low")
#'     )
#'
#'   if(length(tidyselect::eval_select(grouping, .data)) > 0){
#'     .data <- dplyr::left_join(
#'       .data,
#'       corners,
#'       by = dplyr::join_by({{grouping}})
#'     )
#'   } else {
#'     .data <- dplyr::cross_join(
#'       .data,
#'       corners
#'     )
#'   }
#'
#'   .data <- dplyr::mutate(
#'     .data,
#'     !!rlang::sym(norm_names[1]) :=
#'       !!rlang::sym(target_names[1]) / f1_S,
#'
#'     !!rlang::sym(norm_names[2]) :=
#'       !!rlang::sym(target_names[2]) / f2_S
#'   )
#'
#'   .data <- dplyr::relocate(
#'     .data,
#'     tidyselect::all_of(norm_names[1:2] |> rlang::set_names()),
#'     .after = target_pos[2]
#'   )
#'
#'   attr(.data, "norm_procedure") <- "norm_wattfab"
#'   if(!.silent){
#'     wrap_up(
#'       .data,
#'       target_pos,
#'       enquo(.by),
#'       .by_formant,
#'       .names
#'     )
#'   }
#'   return(.data)
#'
#' }
