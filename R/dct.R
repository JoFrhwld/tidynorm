#' Discrete Cosine Transform
#' @param x A vector or matrix to which the discrete cosine transform is applied
#' @param norm_forward DCT normalization (see Details)
#'
#' @details
#' The DCT definitions here are based on the python `scipy.fft.dct` definitions.
#' Specifically this use:
#'
#' ```python
#' # python code
#' scipy.fft.dct(x, orthogonalize = True)
#' ```
#'
#' When `norm_forward = TRUE`
#'
#' \deqn{
#' y_k = \frac{1}{zN} \sum_{j=0}^{N-1}x_j\cos\left(\frac{\pi k(2j+1)}{2N}\right)
#' }
#'
#' \deqn{
#' z = \begin{cases}
#'    \sqrt{2}& \text{for }k=0\\
#'    1 & \text{for }k>0
#'  \end{cases}
#' }
#'
#' When `norm_forward = FALSE`
#' \deqn{
#'  y_k = \frac{2}{z} \sum_{j=0}^{N-1}x_j\cos\left(\frac{\pi k(2j+1)}{2N}\right)
#' }
#'
#' \deqn{
#' z = \begin{cases}
#'    \sqrt{2}& \text{for }k=0\\
#'    1 & \text{for }k>0
#'  \end{cases}
#' }
#'
#' This second formulation is primarily to be able to generate the
#' DCT basis functions like so
#'
#' ```r
#' dct(diag(10), norm_forward = FALSE)
#' ```
#' For the Inverse Discrete Cosine Transform, see [idct].
#'
#' @returns
#' A vector or matrix the same size as `x` containing the
#' Discrete Cosine Transform.
#'
#' @examples
#' x <- seq(0,1, length = 10)
#' y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))
#'
#' dct_coefs <- dct(y)
#'
#' @export
dct <- function(x, norm_forward = TRUE) {
  UseMethod("dct")
}

#' DCT k
#' @noRd
dct_k <- function(x, k, norm_forward = TRUE){
  N <- length(x)
  j <- (1:N)-1

  denom <- pi * k * ((2*j )+1)
  num <- 2*N

  cos_term <- sum(x * cos(denom/num))

  z <- ifelse(k==0, sqrt(2), 1) * ifelse(norm_forward, N, 1)
  s <- ifelse(norm_forward, 1, 2)

  return((s*cos_term)/z)
}

#' DCT numeric
#' @export
#' @keywords internal
dct.numeric <- function(x, norm_forward = TRUE){

  p <- fftw::planDCT(x, type = 2)
  coefs <- fftw::DCT(x, plan = p)
  coefs[1] <- coefs[1]/sqrt(2)

  if(norm_forward){
    coefs <- (coefs/2)/length(x)
  }

  return(coefs)

  # nk <- length(x)
  # sapply(
  #   0:(nk-1),
  #   \(k) dct_k(x, k, norm_forward = norm_forward)
  # )
}

#' DCT matrix
#' @export
#' @keywords internal
dct.matrix <- function(x, norm_forward = TRUE){

  t(apply(x, MARGIN = 1, \(z) dct.numeric(z, norm_forward = norm_forward)))
}

registerS3method("dct", "numeric", method = dct.numeric)
registerS3method("dct", "matrix", method = dct.matrix)

#' regression based dct
#' @noRd
dct_reg <- function(y, call = caller_env()){

  if(all(is.finite(y))){
    return(dct(y))
  }

  basis <- dct(diag(length(y)), norm_forward = FALSE)
  coefs <- try_fetch(
    stats::coef(stats::lm(y ~ -1 + basis)),
    error = \(cnd) {
      cli_warn(
        c("A DCT failed"),
        parent = cnd
      )
      return(NA)
    }
  )
  coefs <- coefs |> rlang::set_names(nm =NULL)
  coefs <- coefs[is.finite(coefs)]
  return(coefs)
}

#' Inverse Discrete Cosine Transform
#'
#' The Inverse DCT
#'
#' @param y DCT coefficients
#' @param n The desired length of the idct
#'
#' @details
#' See the [dct].
#'
#' @returns
#' A vector with the inverse DCT values
#'
#'
#' @examples
#' x <- seq(0,1, length = 10)
#' y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))
#'
#' dct_coefs <- dct(y)
#' recovered_y <- idct(dct_coefs)
#'
#' plot(y, recovered_y)
#'
#' @export
idct <- function(y, n = length(y)){

  if(n == length(y)){

    p <- fftw::planDCT(n, type = 2)
    y[1] <- y[1] * sqrt(2)
    y <- y * 2 * length(y)
    x <- fftw::IDCT(y, plan = p, type = 2)

  }else{

    basis <- dct(diag(n), norm_forward = FALSE)
    basis <- t(basis[,1:length(y)])
    x <- (y %*% basis)[1,]

  }

  return(x)
}

#' Reframe DCT
#'
#' Reframe data columns using the Discrete Cosine Transform
#'
#' @param .data A data frame
#' @param ... [`<tidy-select>`][dplyr::dplyr_tidy_select] One or more unquoted
#' expressions separated by commas. These should target the vowel formant.
#' @param .by [`<tidy-select>`][dplyr::dplyr_tidy_select] A grouping column.
#' @param .token_id_col [`<tidy-select>`][dplyr::dplyr_tidy_select] The token ID column.
#' @param .time_col A time column.
#' @param .order The number of DCT parameters to return. If `NA`, all DCT
#' parameters will be returned.
#'
#' @details
#' This function will tidily apply the Discrete Cosine Transform with forward
#' normalization (see [dct] for more info) to the targeted columns.
#'
#' ### Identifying tokens
#' The DCT only works on a by-token basis, so there must be a column that
#' uniquely identifies (or, in combination with a `.by` grouping, uniquely
#' identifies) each individual token. This column should be passed to
#' `.token_id_col`.
#'
#' ### Order
#' The number of DCT coefficients to return is defined by `.order`. The default
#' value is 5. Larger numbers will lead to less smoothing when the Inverse
#' DCT is applied (see [idct]). Smaller numbers will lead to more smoothing.
#'
#' If `NA` is passed to `.order`, all DCT parameters will be returned, which
#' when the Inverse DCT is supplied, will completely reconstruct the original
#' data.
#'
#' ### Sorting by Time
#' An optional `.time_col` can also be defined to ensure that the data is
#' correctly arranged by time.
#'
#' @returns
#' A data frame with with the targeted DCT coefficients, along with two
#' additional columns
#'
#' \describe{
#'  \item{.param}{The nth DCT coefficient number}
#'  \item{.n}{The number of original data values}
#' }
#'
#' @example inst/examples/ex-reframe_with_dct.R
#' @export
reframe_with_dct <- function(
    .data,
    ...,
    .token_id_col=NULL,
    .by = NULL,
    .time_col = NULL,
    .order = 5
){
  targets <- expr(...)

  cols = enquos(
    .token_id_col = .token_id_col,
    .time = .time_col,
    .by = .by
  )

  for(col in cols){
    try_fetch(
      tidyselect::eval_select(col, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  order <- if(!is.finite(.order)){
    expr(dplyr::n())
  } else {
    expr(.order)
  }

  # make sure groupings are ok
  check_grouping(.data, {{.by}})


  if(quo_is_null(enquo(.time_col))){
    cli_par()
    cli_inform(
      c(
        "i" = "No {.arg .time_col} provided.",
        "i" = "Assuming {.arg .data} is arranged by time."
      )
    )
    cli_end()
  } else {
    .data <- dplyr::arrange(.data, {{.time_col}}) |>
      dplyr::select(-{{.time_col}})
  }

  grouping_list <- make_dct_grouping(
    .data,
    {{.by}},
    {{.token_id_col}}
  )
  .data <- grouping_list$.data
  by_grouping <- grouping_list$by_grouping
  joining <- grouping_list$joining

  orig <- dplyr::select(
    .data,
    -!!targets
  ) |>
    dplyr::slice(
      .by = !!by_grouping,
      1
    )

  dct_df <- .data |>
    dplyr::mutate(
      .by =!!by_grouping,
      dplyr::across(
        !!targets,
        \(x){
          if(mean(is.finite(x)) < 0.9){
            x <- NA
          }
          return(x)
        }
      )
    ) |>
    dplyr::reframe(
      .by = !!by_grouping,
      .param = (1:!!order)-1,
      dplyr::across(
        !!targets,
        \(x) dct_reg(x)[1:!!order]
      ),
      .n = dplyr::n()
    )

  out_df <- dplyr::left_join(
    orig,
    dct_df,
    by = unique(joining)
  )

  return(out_df)

}

#' Reframe with IDCT
#'
#' Reframe data columns using the Inverse Discrete Cosine Transform
#'
#' @inheritParams reframe_with_dct
#' @param .param_col A column identifying the DCT parameter number
#' @param .n The size of the outcome of the IDCT
#'
#' @details
#' This will apply the Inverse Discrete Cosine Transform to the targeted
#' columns. See [idct].
#'
#' ### Identifying tokens
#' The IDCT only works on a by-token basis, so there must be a column that
#' uniquely identifies (or, in combination with a `.by` grouping, uniquely
#' identifies) each individual token. This column should be passed to
#' `.token_id_col`.
#'
#' ### Size of the output
#' The output of the IDCT can be arbitrarily long as defined by the `.n`
#' argument. `.n` can either be an integer, or an unqoted data column.
#'
#' ### The Parameter Column
#' The order of the DCT parameters is crucially important. The optional
#' `.param_col` will ensure the data is properly arranged.
#'
#' @returns
#' A data frame with the IDCT of the targeted columns along with an
#' additional `.time` column.
#'
#' \describe{
#'  \item{.time}{A column from 1 to `.n` by token}
#' }
#'
#' @example inst/examples/ex-reframe_with_idct.R
#'
#' @export
reframe_with_idct <- function(
    .data,
    ...,
    .token_id_col=NULL,
    .by = NULL,
    .param_col = NULL,
    .n = 20
){
  targets <- expr(c(...))
  cols = enquos(
    .token_id_col = .token_id_col,
    .param_col = .param_col,
    .by = .by
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  for(col in cols){
    try_fetch(
      tidyselect::eval_select(col, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  # make sure groupings are ok
  check_grouping(.data, {{.by}})

  if(quo_is_null(cols$.param_col)){
    cli_par()
    cli_inform(
      c(
        "i" = "No {.arg .param_col} provided.",
        "i" = "Assuming {.arg .data} is arranged by parameter."
      )
    )
    cli_end()
  } else {
    .data <- dplyr::arrange(.data, {{.param_col}}) |>
      dplyr::select(-{{.param_col}})
  }

  grouping_list <- make_dct_grouping(
    .data,
    {{.by}},
    {{.token_id_col}}
  )
  .data <- grouping_list$.data
  by_grouping <- grouping_list$by_grouping
  joining <- grouping_list$joining

  orig <- dplyr::select(
    .data,
    -!!targets
  ) |>
    dplyr::slice(
      .by = !!by_grouping,
      1
    )


  idct_df <- .data |>
    dplyr::reframe(
      .by = !!by_grouping,
      .time = 1:dplyr::first({{.n}}),
      dplyr::across(
        !!targets,
        \(x) idct(x, n = dplyr::first({{.n}}))
      )
    )

  out_df <- dplyr::left_join(
    orig,
    idct_df,
    by = unique(joining)
  )
  return(out_df)

}

#' Reframe as DCT Smooth
#'
#' Apply a DCT Smooth to the targeted data
#'
#' @inheritParams reframe_with_dct
#'
#' @details
#' This is roughly equivalent to applying [reframe_with_dct] followed by
#' [reframe_with_idct]. As long as the value passed to `.order` is less than
#' the length of the each token's data, this will result in a smoothed version
#' of the data.
#'
#' ### Identifying tokens
#' The DCT only works on a by-token basis, so there must be a column that
#' uniquely identifies (or, in combination with a `.by` grouping, uniquely
#' identifies) each individual token. This column should be passed to
#' `.token_id_col`.
#'
#' ### Order
#' The number of DCT coefficients to return is defined by `.order`. The default
#' value is 5. Larger numbers will lead to less smoothing when the Inverse
#' DCT is applied (see [idct]). Smaller numbers will lead to more smoothing.
#'
#' If `NA` is passed to `.order`, all DCT parameters will be returned, which
#' when the Inverse DCT is supplied, will completely reconstruct the original
#' data.
#'
#' ### Sorting by Time
#' An optional `.time_col` can also be defined to ensure that the data is
#' correctly arranged by time.
#'
#' Additionally, if `.time_col` is provided, the original time column will
#' be included in the output
#'
#' @returns
#' A data frame where the target columns have been smoothed using the
#' DCT.
#'
#' @example inst/examples/ex-reframe_with_dct_smooth.R
#'
#'
#' @export
reframe_with_dct_smooth <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .order = 5
){
  targets <- expr(...)
  cols = enquos(
    .token_id_col = .token_id_col,
    .time_col = .time_col,
    .by = .by
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  for(col in cols){
    try_fetch(
      tidyselect::eval_select(col, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  # make sure groupings are ok
  check_grouping(.data, {{.by}})

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
  }


  .dct_data <- reframe_with_dct(
    .data,
    !!targets,
    .token_id_col = {{.token_id_col}},
    .by = !!by_grouping,
    .time_col = {{.time_col}},
    .order = .order
  )


  .dct_smooth <- reframe_with_idct(
    .dct_data,
    !!targets,
    .token_id_col = {{.token_id_col}},
    .by = !!by_grouping,
    .param_col = !!sym(".param"),
    .n = !!sym(".n")
  )

  if(!quo_is_null(cols$.time_col)){
    .dct_smooth <- dplyr::select(
      .dct_smooth,
      -!!sym(".time")
    ) |>
      dplyr::mutate(
        .by = !!by_grouping,
        .row = dplyr::row_number()
      ) |>
      dplyr::left_join(
        .time_data,
        by = unique(c(joining, ".row"))
      )
  }

  return(.dct_smooth)
}


