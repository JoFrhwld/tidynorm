#' @export
dct_q <- quickr::quick(function(
    y,
    kk = length(y),
    jj = length(y),
    forward = TRUE
) {
  declare(
    type(y = double(NA)),
    type(kk = integer(1)),
    type(jj = integer(1)),
    type(pi = double(1)),
    type(forward = logical(1))
  )

  pi = 4*(4*atan(1/5) - atan(1/239))

  k_vec = 0:(kk - 1)
  j_vec = 0:(jj - 1)
  N = 0.0

  bank = matrix(0, nrow = kk, ncol = jj)

  for (j in seq_along(j_vec)) {
    N = N + 1.0
    for (k in seq_along(k_vec)) {
      bank[k,j] = cos((pi * (k - 1) * ((2*(j - 1)) + 1))/(2*jj))
    }
  }

  coefs = double(kk)

  for (i in seq_along(coefs)) {
    coefs[i] = sum(
      bank[i,] * y
    )
  }

  normer = ifelse(forward, 1/N, 2)
  coefs = coefs * normer
  coefs[1] = coefs[1] / sqrt(2)

  coefs
})

