#' Discrete Cosine Transform
#' @param x A vector or matrix to which the discrete cosine transform is applied
#' @param norm_forward DCT normalization (see Details)
#'
#' @details
#' The DCT definition here is based on the python scipy.fft.dct definitions.
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
#' This second formulation is primarilly to be able to generate the
#' DCT basis functions like so
#'
#' ```r
#' dct(diag(10), norm_forward = FALSE)
#' ```
#' For the Inverse Discrete Cosine Transform, see [idct].
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
  nk <- length(x)
  sapply(
    0:(nk-1),
    \(k) dct_k(x, k, norm_forward = norm_forward)
  )
}

#' DCT matrix
#' @export
#' @keywords internal
dct.matrix <- function(x, norm_forward = TRUE){
  t(apply(x, MARGIN = 1, \(z) dct.numeric(z, norm_forward = norm_forward)))
}

registerS3method("dct", "numeric", method = dct.numeric)
registerS3method("dct", "matrix", method = dct.matrix)


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
  basis <- dct(diag(n), norm_forward = FALSE)
  basis <- t(basis[,1:length(y)])
  x <- (y %*% basis)[1,]
  return(x)
}

#' Reframe DCT
#' @param .data A data frame
#' @param ... [`<tidy-select>`][dplyr::dplyr_tidy_select] One or more unquoted
#' expressions separated by commas. These should target the vowel formant
#' @param .token_id_col A token ID column
#' @param .time_col A time column
#' @param .order The DCT order
#'
#' @export
reframe_dct <- function(
    .data,
    ...,
    .token_id_col,
    .by = NULL,
    .time_col = NULL,
    .order = 5
){
  targets <- expr(c(...))
  tokens <- enquo(.token_id_col)
  time <- enquo(.time_col)
  grouping <- enquo(.by)

  ## a token column is required
  tokens_pos <- try_fetch(
    tidyselect::eval_select(tokens, data = .data),
    error = \(cnd) selection_errors(cnd)
  )
  check_tokens(tokens_pos)

  # make sure groupings are ok
  check_grouping(.data, grouping)
  group_pos <- tidyselect::eval_select(
    grouping, data = .data
  )

  target_pos <- try_fetch(
    tidyselect::eval_select(targets, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  time_pos <- try_fetch(
    tidyselect::eval_select(time, data = .data),
    error = \(cnd) selection_errors(cnd)
  )

  if(length(time_pos) < 1){
    cli_par()
    cli_inform(
      c(
        "i" = "No {.arg .time_col} provided.",
        "i" = "Assuming {.arg .data} is arranged by time."
      )
    )
    cli_end()
  } else {
    .data <- dplyr::arrange(.data, !!time) |>
      select(-!!time)
  }

  orig <- dplyr::select(
    .data,
    -!!targets
  ) |>
    dplyr::ungroup() |>
    dplyr::slice(
      .by = c(!!tokens,!!grouping),
      1
    )

  dct_df <- dplyr::ungroup(
    .data
  ) |>
    dplyr::reframe(
      .by = c(!!tokens, !!grouping),
      param = (1:.order)-1,
      across(
        !!targets,
        \(x) dct(x)[1:.order]
      )
    )

  joining <- list(tokens)
  if(length(group_pos) > 0){
    joining <- c(joining, list(grouping))
  }

  out_df <- dplyr::left_join(
    orig,
    dct_df,
    by = join_by(
      !!!joining
    )
  )

  return(out_df)

}


