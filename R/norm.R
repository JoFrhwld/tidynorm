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
#' @param .by_formant Whether or not the normalization method is formant
#' intrinsic.
#' @param .L An expression defining the location parameter.
#' See Details for more information.
#' @param .S An expression defining the scale parameter.
#' See Details for more information.
#' @param .pre_trans A function to apply to formant values before normalization.
#' @param .post_trans A function to apply to formant values after normalization.
#' @param .drop_orig Whether or not to drop the original formant data columns.
#' @param .keep_params Whether or not to keep the Location (`*_.L`) and Scale
#' (`*_.S`) normalization parameters
#' @param .names A [glue::glue()] expression for naming the normalized
#' data columns. The `"{.formant}"` portion corresponds to the name of the original
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
#' - [norm_wattfab]
#'
#' ### Location and Scale expressions
#' All normalization procedures built on [norm_generic] produce normalized
#' formant values (\eqn{\hat{F}}) by subtracting a location parameter
#' (\eqn{L}) and dividing by a scale parameter (\eqn{S}).
#'
#' \deqn{
#' \hat{F} = \frac{F-L}{S}
#' }
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
#' ### Pre and Post normalization transforms
#' To apply any transformations before or after normalization,
#' you can pass a function to `.pre_trans` and `.post_trans`.
#'
#' ### Formant In/Extrinsic Normalization
#' If `.by_formant` is `TRUE`, normalization will be formant intrinsic.
#' if `.by_formant` is `FALSE`, normalization will be extrinsic.
#'
#' @example inst/examples/ex-norm_generic.R
#' @export
norm_generic <- function(
  .data,
  ...,
  .by = NULL,
  .by_formant = FALSE,
  .L = 0,
  .S = 1,
  .pre_trans = \(x)x,
  .post_trans = \(x)x,
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.formant}_n",
  .silent = FALSE,
  .call = caller_env()
){

  targets <- expr(...)
  cols <- enquos(
    .by = .by
  )

  .names2 <- glue::glue(.names, .formant = ".formant")

  if(env_name(.call) == "global"){
    .call <- current_env()
  }

  prev_attr <- attributes(.data)$norminfo

  check_grouping(.data, {{.by}}, call = .call)

  #grouping <- rlang::enquo(.by)
  group_pos <- tidyselect::eval_select(
    enquo(.by), data = .data
  )

  # evaluating for the number of
  # targeted columns
  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd, arg = "...", call = .call)
  )

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
    names_to = ".formant_name",
    values_to = ".formant"
  ) |>
    dplyr::mutate(
      .formant_num = stringr::str_extract(
        !!sym(".formant_name"),
        r"{[fF](\d)}",
        group = 1
      ) |> as.numeric(),
      .formant = .pre_trans(.formant)
    )

  # see if the data is grouped
  grouped_by <- dplyr::group_vars(
    .data
  )

  # augment grouping as necessary to
  # match .by_formant
  formant_symbol <- quo(NULL)
  if(.by_formant & length(grouped_by > 0)){
    .data <- dplyr::group_by(
      .data,
      !!sym(".formant_name"),
      .add = TRUE
    )
  } else if(.by_formant){
    formant_symbol <- sym(".formant_name")
  }

  .data <- dplyr::mutate(
    .data,
    .by = c({{.by}}, !!formant_symbol),
    .L = {{.L}},
    .S = {{.S}},
    "{.names2}" := .post_trans((.formant - .L) / .S),
    .formant = .post_trans(.formant)
  )

  # set up value columns for pivoting
  # back wide
  if(!.keep_params){
    .data <- dplyr::select(
      .data,
      -c(!!sym(".L"), !!sym(".S"))
    )
  }
  .data <- dplyr::select(
    .data,
    -!!sym(".formant_num")
  )

  # pivot_back wide
  .data <- tidyr::pivot_wider(
    .data,
    names_from = !!sym(".formant_name"),
    values_from = c(
      tidyselect::starts_with(".formant") & !tidyselect::all_of(".formant_name"),
      tidyselect::any_of(c(".L", ".S"))
      ),
    names_glue = "{.formant_name}_{.value}"
  )

  # move normalized columns adjacent to
  # original
  .data <- dplyr::relocate(
    .data,
    c(
      tidyselect::matches("_.formant"),
      tidyselect::ends_with("_.L"),
      tidyselect::ends_with("_.S")
    ),
    .before = target_pos[1]
  )

  # remove _.col from names
  .data <- dplyr::rename_with(
    .data,
    .cols = tidyselect::matches("_.formant"),
    .fn = \(x) stringr::str_remove(x, "_.formant")
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
        .norm_cols = glue::glue(.names, .formant = names(target_pos)),
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
#'   \hat{F}_{ij} = \frac{F_{ij} - L_i}{S_i}
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
#' Where
#' - \eqn{\hat{F}} is the normalized formant
#' - \eqn{i} is the formant number
#' - \eqn{j} is the token number
#'
#' @references
#' Lobanov, B. (1971). Classification of Russian vowels spoken by different listeners.
#' Journal of the Acoustical Society of America, 49, 606–608.
#'
#' @example inst/examples/ex-norm_lobanov.R
#' @export
norm_lobanov <- function(
    .data,
    ...,
    .by = NULL,
    .by_formant = TRUE,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.formant}_z",
    .silent = FALSE
){

  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .L = mean(!!sym(".formant"), na.rm = T),
    .S = sd(!!sym(".formant"), na.rm = T),
    .by_formant = TRUE,
    .drop_orig = .drop_orig,
    .keep_params = .keep_params,
    .names = .names,
    .silent = .silent
  )

  # norminfo <- attr(.data, "norminfo")
  # norminfo[[length(norminfo)]]$norm_procedure <- "norm_lobanov"
  # attr(.data, "norminfo") <- norminfo

  return(.data)

}

#' Nearey Normalize
#' @inheritParams norm_generic
#' @importFrom rlang `!!`
#'
#' @details
#' When formant extrinsic:
#' \deqn{
#'  \hat{F}_{ij} = \log(F_{ij}) - L
#' }
#' \deqn{
#'  L = \frac{1}{MN}\sum_{i=1}^M\sum_{j=1}^N \log(F_{ij})
#' }
#'
#' When formant intrinsic:
#' \deqn{
#'  \hat{F}_{ij} = \log(F_{ij}) - L_{i}
#' }
#'
#' \deqn{
#'   L_i = \frac{1}{N}\sum_{j=1}^{N}\log(F_{ij})
#' }
#'
#' Where
#' - \eqn{\hat{F}} is the normalized formant
#' - \eqn{i} is the formant number
#' - \eqn{j} is the token number
#'
#' @example inst/examples/ex-norm_nearey.R
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
    .names = "{.formant}_lm",
    .silent = FALSE
){
  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .pre_trans = log,
    .post_trans = \(x)x,
    .L = mean(.formant, na.rm = T),
    .S = 1,
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

#' Delta F Normalize
#' @inheritParams norm_generic
#'
#' @param .by_formant Ignored by this procedure
#'
#' @details
#' \deqn{
#'  \hat{F}_{ij} = \frac{F_{ij}}{S}
#' }
#' \deqn{
#'  S = \frac{1}{MN}\sum_{i=1}^M\sum_{j=1}^N \frac{F_{ij}}{i-0.5}
#' }
#'
#' Where
#' - \eqn{\hat{F}} is the normalized formant
#' - \eqn{i} is the formant number
#' - \eqn{j} is the token number
#'
#' @references
#' Johnson, K. (2020). The ΔF method of vocal tract length normalization for vowels.
#' Laboratory Phonology: Journal of the Association for Laboratory Phonology, 11(1),
#' Article 1. [https://doi.org/10.5334/labphon.196](https://doi.org/10.5334/labphon.196)
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
    .names = "{.formant}_df",
    .silent = FALSE
){
  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .L = 0,
    .S = mean(.formant/(.formant_num-0.5), na.rm = T),
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

#' Watt & Fabricius method
#' @inheritParams norm_generic
#' @param .by_formant Ignored by this procedure
#'
#' @details
#' This is a modified version of the Watt & Fabricius Method. The original
#' method identified point vowels over which F1 and F2 centroids were calculated.
#' The procedure here just identifies centroids by taking the mean of
#' all formant values.
#'
#' \deqn{
#' \hat{F}_{ij} = \frac{F_{ij}}{S_i}
#' }
#'
#' \deqn{
#'  S_i = \frac{1}{N}\sum_{j=1}^N F_{ij}
#' }
#'
#' Where
#' - \eqn{\hat{F}} is the normalized formant
#' - \eqn{i} is the formant number
#' - \eqn{j} is the token number
#'
#' @references
#' Watt, D., & Fabricius, A. (2002). Evaluation of a technique for improving the
#' mapping of multiple speakers’ vowel spaces in the F1 ~ F2 plane.
#' Leeds Working Papers in Linguistics and Phonetics, 9, 159–173.
#'
#' @example inst/examples/ex-norm_wattfab.R
#'
#' @export
norm_wattfab <- function(
    .data,
    ...,
    .by = TRUE,
    .by_formant = TRUE,
    .drop_orig = FALSE,
    .keep_params = FALSE,
    .names = "{.formant}_wf",
    .silent = FALSE
){
  targets <- rlang::expr(c(...))

  .data <- norm_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .L = 0,
    .S = mean(.formant, na.rm = T),
    .by_formant = TRUE,
    .drop_orig = .drop_orig,
    .keep_params = .keep_params,
    .names = .names,
    .silent = .silent
  )

  norminfo <- attr(.data, "norminfo")
  norminfo[[length(norminfo)]]$norm_procedure <- "norm_wattfab"
  attr(.data, "norminfo") <- norminfo

  return(.data)
}
