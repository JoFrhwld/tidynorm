# generate a curve
x <- seq(0, 1, length = 10)
y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

test_that("dct coefs from package are equal to established library", {

  # Only run test if scipy is available
  skip_if(!require(reticulate))
  reticulate::py_config()
  skip_if(!reticulate::py_available())
  skip_if(!"scipy" %in% reticulate::py_list_packages()$package)

  # import scipy dct implementation
  scipy <- reticulate::import("scipy")
  py_dct <- scipy$fft$dct

  # get scipy coefficients & strip dim() to match R vector
  py_coefs <- py_dct(y, norm = "forward", orthogonalize = TRUE)
  dim(py_coefs) <- NULL

  # package dct
  r_coefs <- tidynorm::dct(y)

  expect_equal(length(r_coefs), length(py_coefs))
  expect_equal(r_coefs, py_coefs)

})


test_that("idct recovery", {
  coefs <- dct(y)
  recovered <- idct(coefs)

  expect_equal(recovered, y)
})

test_that("dct matrix method", {
  mat <- cbind(y, y/3)
  coefs <- dct(mat)

  expect_equal(
    dim(coefs),
    dim(mat)
  )

  expect_equal(
    dimnames(coefs),
    dimnames(mat)
  )

})

test_that("idct matrix method", {
  mat <- cbind(y, y/3)
  coefs <- dct(mat)
  recovered <- idct(coefs)

  expect_equal(
    recovered,
    mat
  )

})
