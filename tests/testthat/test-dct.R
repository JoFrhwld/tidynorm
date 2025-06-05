test_that("dct coefs", {

  skip_if(!require(reticulate))

  reticulate::py_config()
  skip_if(!reticulate::py_available())

  skip_if(!"scipy" %in% reticulate::py_list_packages()$package)

  scipy <- reticulate::import("scipy")
  py_dct <- scipy$fft$dct

  x <- seq(0, 1, length = 10)
  y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

  py_coefs <- py_dct(y, norm = "forward", orthogonalize = TRUE)
  r_coefs <- tidynorm::dct(y)

  expect_equal(length(r_coefs), length(py_coefs))

})
