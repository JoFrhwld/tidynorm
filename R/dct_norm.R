#' Generic Formant Track Normalization Procedure
#'
#' Normalize formant tracks using Discrete Cosine Transform normalization
#'
#' @param .data A data frame containing vowel formant data
#' @param ... [`<tidy-select>`][dplyr::dplyr_tidy_select] One or more unquoted
#' expressions separated by commas. These should target the vowel formant
#' data columns.
#' @param .by  [`<tidy-select>`][dplyr::dplyr_tidy_select] A selection of
#' columns to group by. Typically a column of speaker IDs.
#' @param .token_id_col [`<data-masking>`][rlang::args_data_masking] A column
#' that identifies token ids.
#' @param .by_formant Whether or not the normalization method is formant
#' intrinsic.
#' @param .by_token Whether or not the normalization method is token
#' intrinsic
#' @param .L An expression defining the location parameter.
#' See Details for more information.
#' @param .S An expression defining the scale parameter.
#' See Details for more information.
#' @param .pre_trans A function to apply to formant values before normalization.
#' @param .post_trans A function to apply to formant values after normalization.
#' @param .time_col [`<data-masking>`][rlang::args_data_masking] A time column.
#' (optional)
#' @param .order The number of DCT parameters to use.
#' @param .names A [glue::glue()] expression for naming the normalized
#' data columns. The `"{.formant}"` portion corresponds to the name of the original
#' formant columns.
#' @param .drop_orig Should the originally targeted columns be dropped.
#' @param .return_dct Whether or not the normalized DCT coefficients themselves
#' should be returned.
#' @param .silent Whether or not the informational message should be printed.
#' @param .call Used for internal purposes.
#'
#' @details
#' This will normalize vowel formant tracks in the following steps:
#'
#' 1. Any `.pre_trans` transformations will be applied to the formant data.
#' 2. The Discrete Cosine Transform will be applied to the formant data.
#' 3. Location `.L` and Scale `.S` expressions will be used to summarize the zero<sup>th</sup>
#' DCT coefficients.
#' 4. These location and scale will be used to normalize the DCT coefficients.
#' 5. If `.return_dct = TRUE`, these normalized DCT coefficients will be returned.
#' Otherwise, the Inverse Discrete Cosine Transform will be applied to the
#' normalized DCT coefficients.
#' 6. Any `.post_trans` transformations will be applied.
#'
#' ### Location and Scale expressions
#' All normalization procedures built on [norm_track_generic] work by normalizing
#' DCT coefficients directly. If \eqn{F_k} is the k<sup>th</sup> DCT coefficient
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
#' by \eqn{\sqrt{2}}, this is done by [norm_track_generic] itself, to allow greater
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
#' The DCT only works on a by-token basis, so there must be a column that
#' uniquely identifies (or, in combination with a `.by` grouping, uniquely
#' identifies) each individual token. This column should be passed to
#' `.token_id_col`.
#'
#' ### Order
#' The number of DCT coefficients used is defined by `.order`. The default
#' value is 5. Larger numbers will lead to less smoothing, and smaller numbers
#' will lead to more smoothing.
#'
#' @returns
#' A data frame of normalized formant tracks.
#'
#' @example inst/examples/ex-norm_track_generic.R
#'
#' @export
norm_track_generic <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .by_formant = FALSE,
    .by_token = FALSE,
    .time_col = NULL,
    .L = 0,
    .S = 1/sqrt(2),
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .names = "{.formant}_n",
    .silent = FALSE,
    .call = caller_env()
){
  if(env_name(.call) == "global"){
    .call2 = current_env()
  }
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls, .call2)

  prev_attr <- attributes(.data)$norminfo

  .names2 <- glue::glue(.names, .formant=".formant")

  targets <- expr(c(...))
  target_pos <- tidyselect::eval_select(targets, .data)
  cols <- enquos(
    .by = .by,
    .token_id_col = .token_id_col,
    .time_col = .time_col
  )

  for(x in cols){
    try_fetch(
      tidyselect::eval_select(x, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  check_grouping(.data, {{.by}}, .call)

  grouping_list <- make_dct_grouping(
    .data,
    {{.by}},
    {{.token_id_col}}
  )
  .data <- grouping_list$.data
  by_grouping <- grouping_list$by_grouping
  joining <- grouping_list$joining

  if(!quo_is_null(cols$.time_col)){
    .time_data <- dplyr::arrange(.data, {{.time_col}}) |>
      dplyr::select(
        {{.by}},
        {{.token_id_col}},
        {{.time_col}},
        dplyr::group_cols()
      ) |>
      dplyr::mutate(
        .by = !!by_grouping,
        .row = dplyr::row_number()
      )

    .data <- dplyr::mutate(
      .data,
      .by = !!by_grouping,
      .row = dplyr::row_number()
    )
  }

  .dct_data <- .data |>
    dplyr::mutate(
      across(
        !!targets,
        .fns=\(x) .pre_trans(x)
      )
    ) |>
    reframe_with_dct(
      !!targets,
      .token_id_col = {{.token_id_col}},
      .by = !!by_grouping,
      .time_col = {{.time_col}},
      .order = .order
    )

  normed <- norm_dct_generic(
    .dct_data,
    !!targets,
    .token_id_col = {{.token_id_col}},
    .by = {{.by}},
    .by_formant = .by_formant,
    .by_token = .by_token,
    .param_col = !!sym(".param"),
    .L = {{.L}},
    .S = {{.S}},
    .names = .names2,
    .silent = FALSE,
    .drop_orig = .drop_orig,
    .call = current_env()
  )

  if(.return_dct){
    normed <- normed |>
      dplyr::rename_with(
        .fn = \(x) stringr::str_remove(x, "_.formant")
      )
    return(normed)
  }

  normed_track <- reframe_with_idct(
    normed,
    matches("_.formant"),
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .param_col = !!sym(".param"),
    .n = !!sym(".n")
  ) |>
    dplyr::mutate(
      dplyr::across(
        matches("_.formant"),
        .fns = \(x) .post_trans(x)
      )
    )

  if(!quo_is_null(cols$.time_col)){
    normed_track <- normed_track |>
      dplyr::select(
        -sym(".time")
      ) |>
      dplyr::mutate(
        .by = !!by_grouping,
        .row = dplyr::row_number()
      ) |>
      left_join(
        .time_data,
        by = c(joining, ".row")
      )
  }

  normed_track <- normed_track |>
    dplyr::rename_with(
      .fn = \(x) stringr::str_remove(x, "_.formant")
    )

  return(normed_track)
}

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
    .S = 1/sqrt(2),
    .by_formant = FALSE,
    .by_token = FALSE,
    .names = "{.formant}_n",
    .silent = FALSE,
    .drop_orig = FALSE,
    .call = caller_env()
){
  if(env_name(.call) == "global"){
    .call2 = current_env()
  }
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls, .call2)

  .names <- glue::glue(.names, .formant=".formant")

  targets <- expr(c(...))
  cols <- enquos(
    .by = .by,
    .token_id_col = .token_id_col,
    .param_col = .param_col
  )

  for(x in cols){
    try_fetch(
      tidyselect::eval_select(x, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  check_grouping(.data, {{.by}}, .call)

  grouping_list <- make_dct_grouping(
    .data,
    {{.by}},
    {{.token_id_col}}
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
  #byformant <- NULL
  if(.by_formant & grouped_by){
    .dct_data <- dplyr::group_by(
      .dct_data,
      !!sym(".formant_name"),
      .add = TRUE
    )
    by_grouping2 <- expr(NULL)
    by_grouping_noid <- expr(NULL)
  }else if (.by_token & grouped_by){
    .dct_data <- dplyr::group_by(
      .dct_data,
      {{.token_id_col}},
      .add = TRUE
    )
    by_grouping2 <- expr(NULL)
    by_grouping_noid <- expr(NULL)
  } else if(grouped_by){
    by_grouping2 <- expr(NULL)
    by_grouping_noid <- expr(NULL)
  }else if(.by_formant){
    by_grouping2 <- expr(c(!!by_grouping, sym(".formant_name")))
    by_grouping_noid <- expr(c({{.by}}, sym(".formant_name")))
  }else if(.by_token){
    by_grouping2 <- expr(c(!!by_grouping, {{.token_id_col}}))
    by_grouping_noid <- expr(c({{.by}}, {{.token_id_col}}))
  } else {
    by_grouping2 <- by_grouping
    by_grouping_noid <- expr({{.by}})
  }

  zeroth <- dplyr::filter(
    .dct_data,
    .by = !!by_grouping2 ,
    !!sym(".param") == min(!!sym(".param"))
  )

  if(grouped_by & !.by_token){
    zeroth <- zeroth |>
      dplyr::ungroup({{.token_id_col}})
  }

  if(grouped_by){
    keepit <- "keep"
  }else{
    keepit <- NULL
  }

  norm_params <- zeroth |>
    dplyr::summarise(
      .by = !!by_grouping_noid,
      .L = {{.L}},
      .S = {{.S}} * sqrt(2),
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
        !!sym(".param") == 0 ~ (!!sym(".formant") - !!sym(".L"))/!!sym(".S"),
        .default = !!sym(".formant")/!!sym(".S")
      )
    ) |>
    dplyr::select(
      -c(
        !!sym(".L"),
        !!sym(".S"),
        !!sym(".formant_num")
      )
    )

  if(.drop_orig){
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

  if(!"norm_track_generic(...)" %in% call_tree){
    normed <- normed |>
      dplyr::rename_with(
        .fn = \(x) stringr::str_remove(x, "_.formant")
      )
  }

  return(normed)

}

#' Lobanov Track Normalization
#'
#' @inheritParams norm_track_generic
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
#' @returns
#' A data frame of Lobanov normalized formant tracks.
#'
#' @references
#' Lobanov, B. (1971). Classification of Russian vowels spoken by different listeners.
#' Journal of the Acoustical Society of America, 49, 606–608.
#'
#' @example inst/examples/ex-norm_track_lobanov.R
#' @export
norm_track_lobanov <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .names = "{.formant}_z",
    .silent = FALSE
){
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls)

  targets <- expr(...)
  normed <- norm_track_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .by_formant = TRUE,
    .L = mean(!!sym(".formant"), na.rm = T),
    .S = sd(!!sym(".formant"), na.rm = T),
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .time_col = {{.time_col}},
    .order = .order,
    .return_dct = .return_dct,
    .drop_orig = .drop_orig,
    .names = .names,
    .silent = .silent
  )
  return(normed)
}

#' Nearey Track Normalization
#'
#' @inheritParams norm_track_generic
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
#' @returns
#' A data frame of Nearey normalized formant tracks.
#'
#' @example inst/examples/ex-norm_track_nearey.R
#'
#' @references
#' Nearey, T. M. (1978). Phonetic Feature Systems for Vowels \[Ph.D.\].
#' University of Alberta.
#' @export
norm_track_nearey <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .by_formant = FALSE,
    .time_col = NULL,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .names = "{.formant}_lm",
    .silent = FALSE
){
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls)

  targets <- expr(...)
  normed <- norm_track_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .by_formant = .by_formant,
    .L = mean(!!sym(".formant"), na.rm = T),
    .S = 1/sqrt(2),
    .pre_trans = log,
    .post_trans = \(x)x,
    .time_col = {{.time_col}},
    .order = .order,
    .return_dct = .return_dct,
    .drop_orig = .drop_orig,
    .names = .names,
    .silent = .silent
  )
  return(normed)
}



#' Delta F Track Normalization
#'
#' @inheritParams norm_track_generic
#'
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
#' @returns
#' A data frame of Delta F normalized formant tracks.
#'
#' @example inst/examples/ex-norm_track_deltaF.R
#'
#' @references
#' Johnson, K. (2020). The ΔF method of vocal tract length normalization for vowels.
#' Laboratory Phonology: Journal of the Association for Laboratory Phonology, 11(1),
#' Article 1. [https://doi.org/10.5334/labphon.196](https://doi.org/10.5334/labphon.196)
#'
#' @export
norm_track_deltaF <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .names = "{.formant}_df",
    .silent = FALSE
){
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls)

  targets <- expr(...)
  normed <- norm_track_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .by_formant = FALSE,
    .L = 0,
    .S = mean(!!sym(".formant")/(!!sym(".formant_num") - 0.5), na.rm = T),
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .time_col = {{.time_col}},
    .order = .order,
    .return_dct = .return_dct,
    .drop_orig = .drop_orig,
    .names = .names,
    .silent = .silent
  )
  return(normed)
}


#' Watt and Fabricius Track normalization
#'
#' @inheritParams norm_track_generic
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
#' @returns
#' A data frame of Watt & Fabricius normalized formant tracks.
#'
#' @example inst/examples/ex-norm_track_wattfab.R
#'
#' @references
#' Watt, D., & Fabricius, A. (2002). Evaluation of a technique for improving the
#' mapping of multiple speakers’ vowel spaces in the F1 ~ F2 plane.
#' Leeds Working Papers in Linguistics and Phonetics, 9, 159–173.
#' @export
norm_track_wattfab <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .names = "{.formant}_wf",
    .silent = FALSE
){
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls)

  targets <- expr(...)
  normed <- norm_track_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .by_formant = TRUE,
    .L = 0,
    .S = mean(!!sym(".formant"), na.rm = T),
    .pre_trans = \(x)x,
    .post_trans = \(x)x,
    .time_col = {{.time_col}},
    .order = .order,
    .return_dct = .return_dct,
    .drop_orig = .drop_orig,
    .names = .names,
    .silent = .silent
  )
  return(normed)
}

#' Bark Difference Track Normalization
#' @inheritParams norm_track_generic
#' @details
#' This is a within-token normalization technique. First all formants are
#' converted to Bark (see [hz_to_bark]), then, within each token, F3 is
#' subtracted from F1 and F2.
#'
#' \deqn{
#' \hat{F}_{ij} = F_{ij} - L_j
#' }
#'
#' \deqn{
#' L_j = F_{3j}
#' }
#'
#' @returns
#' A data frame of either normalized formant tracks, or normalized DCT
#' parameters.
#'
#' @returns
#' A data frame of Watt & Fabricius normalized formant tracks.
#'
#' @references
#' Syrdal, A. K., & Gopal, H. S. (1986). A perceptual model of vowel
#' recognition based on the auditory representation of American English vowels.
#' The Journal of the Acoustical Society of America, 79(4), 1086–1100.
#' [https://doi.org/10.1121/1.393381](https://doi.org/10.1121/1.393381)
#'
#' @example inst/examples/ex-norm_track_barkz.R
#'
#' @export
norm_track_barkz <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .order = 5,
    .return_dct = FALSE,
    .drop_orig = FALSE,
    .names = "{.formant}_bz",
    .silent = FALSE
){
  args <- names(call_match())
  fmls <- names(fn_fmls())
  check_args(args, fmls)

  targets <- expr(...)
  normed <- norm_track_generic(
    .data,
    !!targets,
    .by = {{.by}},
    .token_id_col = {{.token_id_col}},
    .by_formant = FALSE,
    .by_token = TRUE,
    .L = (!!sym(".formant"))[3],
    .S = 1/sqrt(2),
    .pre_trans = hz_to_bark,
    .post_trans = \(x)x,
    .time_col = {{.time_col}},
    .order = .order,
    .return_dct = .return_dct,
    .drop_orig = .drop_orig,
    .names = .names,
    .silent = .silent
  )

  return(normed)
}
