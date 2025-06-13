#' Discrete Cosine Transform
#' @param x A vector or matrix to which the discrete cosine transform is applied
#'
#' @details
#' The DCT definitions here are based on the python `scipy.fft.dct` definitions.
#' Specifically this use:
#'
#' ```python
#' # python code
#' scipy.fft.dct(x, norm = "forward", orthogonalize = True)
#' ```
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
#'
#' For the Inverse Discrete Cosine Transform, see [idct].
#'
#' @returns
#' Returned value depends on `x`.
#' @examples
#' x <- seq(0, 1, length = 10)
#' y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))
#'
#' dct_coefs <- dct(y)
#'
#' @export
dct <- function(x) {
  UseMethod("dct")
}

#' @rdname dct
#' @returns
#' When passed a numeric vector, returns a numeric
#' vector the same size as `x` with the DCT Coefficients.
#'
#' @method dct numeric
#' @export
#' @keywords internal
dct.numeric <- function(x) {
  coefs <- dct_fun(x, kk = length(x))[, 1]
  return(coefs)
}

#' @rdname dct
#' @export
#' @returns
#' When passed a matrix, returns a matrix
#' the same size as `x` with the DCT Coefficients.
#' @keywords internal
dct.matrix <- function(x) {
  out <- dct_mat(x, kk = nrow(x))
  colnames(out) <- colnames(x)
  out
}

registerS3method("dct", "numeric", method = dct.numeric)
registerS3method("dct", "matrix", method = dct.matrix)

#' DCT Basis
#'
#' The Discrete Cosine Transform basis functions
#'
#' @param n The length of the basis.
#' @param k The number of basis functions.
#'
#' @details
#' This function will generate the DCT basis functions.
#'
#' @returns
#' A \eqn{n\times k} matrix
#'
#' @examples
#' basis <- dct_basis(100, 5)
#' matplot(basis, type = "l", lty = 1)
#'
#' @export
dct_basis <- function(n, k) {
  cos_bank(n, k)
}

#' Inverse Discrete Cosine Transform
#'
#' The Inverse DCT
#'
#' @param y A vector or matrix of DCT coefficients
#' @param n The desired length of the idct
#'
#' @details
#' Applies the Inverse DCT (see [dct] for more details).
#'
#' \deqn{
#' x_j = \sqrt{2}y_0 + 2\sum_{k=1}^{N-1} y_k \cos\left(\frac{\pi k(2j+1)}{2J}\right)
#' }
#'
#' @returns
#' The returned value depends on the values in `y`.
#'
#'
#' @examples
#' x <- seq(0, 1, length = 10)
#' y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))
#'
#' dct_coefs <- dct(y)
#' recovered_y <- idct(dct_coefs)
#'
#' plot(y, recovered_y)
#'
#' @export
idct <- function(y, n) {
  UseMethod("idct")
}

#' @rdname idct
#' @returns When passed a numeric vector, returns numeric vector of length `n`.
#' @export
#' @method idct numeric
#' @keywords internal
idct.numeric <- function(y, n = length(y)) {
  x <- idct_fun(y, n = n)[, 1]
  return(x)
}

#' @rdname idct
#' @returns
#' When passed a matrix, returns a matrix with `n` rows and the same number of columns as `y`.
#' @export
#' @method idct matrix
#' @keywords internal
idct.matrix <- function(y, n = nrow(y)) {
  x <- idct_mat(y, n = n)
  colnames(x) <- colnames(y)
  return(x)
}

registerS3method("idct", "numeric", method = idct.numeric)
registerS3method("idct", "matrix", method = idct.matrix)

#' Inverse Discrete Cosine Transform Rate
#'
#' The first derivative of the Inverse DCT
#'
#' @param y DCT coefficients
#' @param n The desired length of the idct
#'
#' @details
#' Returns the first derivative (rate of change) of
#' the Inverse DCT (see [dct] for more details).
#'
#' \deqn{
#' \frac{\delta x_j}{\delta j} = -2\frac{\pi k}{J}\sum_{k=1}^{N-1} y_k \sin\left(\frac{\pi k(2j+1)}{2J}\right)
#' }
#'
#' @returns
#' A vector with the first derivative
#' of the inverse DCT
#'
#'
#' @examples
#' x <- seq(0, 1, length = 10)
#' y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))
#'
#' dct_coefs <- dct(y)
#' y_rate <- idct_rate(dct_coefs)
#'
#' plot(y)
#' plot(y_rate)
#'
#' @export
idct_rate <- function(y, n = length(y)) {
  x <- idct_prime(y, n = n)
  return(x)
}

#' Inverse Discrete Cosine Transform Acceleration
#'
#' The second derivative of the Inverse DCT
#'
#' @param y DCT coefficients
#' @param n The desired length of the idct
#'
#' @details
#' Returns the second derivative (acceleration) of
#' the Inverse DCT (see [dct] for more details).
#'
#' \deqn{
#' \frac{\delta^2 x_j}{\delta j^2} = -2\left(\frac{\pi k}{J}\right)^2\sum_{k=1}^{N-1} y_k \cos\left(\frac{\pi k(2j+1)}{2J}\right)
#' }
#'
#' @returns
#' A vector with the second derivative
#' of the inverse DCT
#'
#'
#' @examples
#' x <- seq(0, 1, length = 10)
#' y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))
#'
#' dct_coefs <- dct(y)
#' y_accel <- idct_accel(dct_coefs)
#'
#' plot(y)
#' plot(y_accel)
#'
#' @export
idct_accel <- function(y, n = length(y)) {
  x <- idct_dprime(y, n = n)
  return(x)
}

#' Reframe with DCT
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
    .token_id_col = NULL,
    .by = NULL,
    .time_col = NULL,
    .order = 5) {
  targets <- expr(...)

  cols <- enquos(
    .token_id_col = .token_id_col,
    .time = .time_col,
    .by = .by
  )

  for (col in cols) {
    try_fetch(
      tidyselect::eval_select(col, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  order <- if (!is.finite(.order)) {
    expr(dplyr::n())
  } else {
    expr(.order)
  }

  # make sure groupings are ok
  check_grouping(.data, {{ .by }})


  if (quo_is_null(enquo(.time_col))) {
    cli_par()
    cli_inform(
      c(
        "i" = "No {.arg .time_col} provided.",
        "i" = "Assuming {.arg .data} is arranged by time."
      )
    )
    cli_end()
  } else {
    .data <- dplyr::arrange(.data, {{ .time_col }}) |>
      dplyr::select(-{{ .time_col }})
  }

  grouping_list <- make_dct_grouping(
    .data,
    {{ .by }},
    {{ .token_id_col }}
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
      .by = !!by_grouping,
      dplyr::across(
        !!targets,
        \(x){
          if (mean(is.finite(x)) < 0.9) {
            x <- NA
          }
          return(x)
        }
      )
    ) |>
    dplyr::reframe(
      .by = !!by_grouping,
      .param = (1:!!order) - 1,
      dplyr::across(
        !!targets,
        \(x) dct(x)[1:!!order]
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
#' @param .rate Whether or not to include the rate of change of signal.
#' @param .accel Whether or not to include acceleration of signal.
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
    .token_id_col = NULL,
    .by = NULL,
    .param_col = NULL,
    .n = 20,
    .rate = FALSE,
    .accel = FALSE) {
  targets <- expr(c(...))
  cols <- enquos(
    .token_id_col = .token_id_col,
    .param_col = .param_col,
    .by = .by
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  for (col in cols) {
    try_fetch(
      tidyselect::eval_select(col, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  # make sure groupings are ok
  check_grouping(.data, {{ .by }})

  if (quo_is_null(cols$.param_col)) {
    cli_par()
    cli_inform(
      c(
        "i" = "No {.arg .param_col} provided.",
        "i" = "Assuming {.arg .data} is arranged by parameter."
      )
    )
    cli_end()
  } else {
    .data <- dplyr::arrange(.data, {{ .param_col }}) |>
      dplyr::select(-{{ .param_col }})
  }

  grouping_list <- make_dct_grouping(
    .data,
    {{ .by }},
    {{ .token_id_col }}
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

  if (!.rate & !.accel) {
    idct_df <- .data |>
      dplyr::reframe(
        .by = !!by_grouping,
        .time = 1:dplyr::first({{ .n }}),
        dplyr::across(
          !!targets,
          \(x) idct(x, n = {{ .n }}[1])
        )
      )
  } else {
    idct_operation <- list(
      s = \(x) idct(x, n = {{ .n }}[1])
    )
    if (.rate) {
      idct_operation$rate <- \(x) idct_prime(x, {{ .n }}[1])
    }
    if (.accel) {
      idct_operation$accel <- \(x) idct_dprime(x, n = {{ .n }}[1])
    }

    idct_df <- .data |>
      dplyr::reframe(
        .by = !!by_grouping,
        .time = 1:({{ .n }}[1]),
        dplyr::across(
          !!targets,
          idct_operation
        )
      )
  }

  out_df <- dplyr::left_join(
    orig,
    idct_df,
    by = unique(joining)
  )
  return(out_df)
}

#' Reframe with DCT Smooth
#'
#' Apply a DCT Smooth to the targeted data
#'
#' @inheritParams reframe_with_dct
#' @inheritParams reframe_with_idct
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
#' DCT, as well as the signal rate of change and acceleration,
#' if requested.
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
    .order = 5,
    .rate = FALSE,
    .accel = FALSE) {
  targets <- expr(...)
  cols <- enquos(
    .token_id_col = .token_id_col,
    .time_col = .time_col,
    .by = .by
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  for (col in cols) {
    try_fetch(
      tidyselect::eval_select(col, data = .data),
      error = \(cnd) selection_errors(cnd)
    )
  }

  # make sure groupings are ok
  check_grouping(.data, {{ .by }})

  grouping_list <- make_dct_grouping(
    .data,
    {{ .by }},
    {{ .token_id_col }}
  )

  .data <- grouping_list$.data
  by_grouping <- grouping_list$by_grouping
  joining <- grouping_list$joining

  if (!quo_is_null(cols$.time_col)) {
    .time_data <- dplyr::arrange(.data, {{ .time_col }}) |>
      dplyr::select(
        {{ .by }},
        {{ .token_id_col }},
        {{ .time_col }},
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
    .token_id_col = {{ .token_id_col }},
    .by = !!by_grouping,
    .time_col = {{ .time_col }},
    .order = .order
  )

  .dct_smooth <- reframe_with_idct(
    .dct_data,
    !!targets,
    .token_id_col = {{ .token_id_col }},
    .by = !!by_grouping,
    .param_col = !!sym(".param"),
    .rate = .rate,
    .accel = .accel
  )

  if (!quo_is_null(cols$.time_col)) {
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
