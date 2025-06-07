# generate a curve
x <- seq(0, 1, length = 10)
y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

test_that("dct coefs from package are equal to established library", {
  # Only run test if scipy is available
  skip_if(!rlang::is_installed("reticulate"))
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
  mat <- cbind(y, y / 3)
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
  mat <- cbind(y, y / 3)
  coefs <- dct(mat)
  recovered <- idct(coefs)

  expect_equal(
    recovered,
    mat
  )
})


test_that("dct-basis works", {
  rows <- 100
  cols <- 5
  basis <- dct_basis(rows, cols)

  expect_equal(
    nrow(basis),
    rows
  )

  expect_equal(
    ncol(basis),
    cols
  )

  expect_equal(
    basis[1, 1],
    sqrt(2)
  )
})

test_that("reframe with dct works", {
  test_order <- 5

  n_tokens <- speaker_tracks |>
    dplyr::count(
      speaker, id
    ) |>
    nrow()

  speaker_dct1 <- reframe_with_dct(
    .data = speaker_tracks,
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .order = test_order
  )

  speaker_dct2 <- reframe_with_dct(
    .data = speaker_tracks,
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .order = NA
  )

  expect_equal(
    nrow(speaker_dct1),
    n_tokens * test_order
  )

  expect_equal(
    nrow(speaker_dct2),
    nrow(speaker_tracks)
  )

  expect_contains(
    colnames(speaker_dct1),
    c("F1", "F2", "F3")
  )
})

test_that("reframe with idct works", {
  out_len <- 20

  n_tokens <- speaker_tracks |>
    dplyr::count(
      speaker, id
    ) |>
    nrow()

  speaker_dct <- reframe_with_dct(
    .data = speaker_tracks,
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )

  speaker_idct <- reframe_with_idct(
    .data = speaker_dct,
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param,
    .n = out_len
  )

  expect_equal(
    nrow(speaker_idct),
    n_tokens * out_len
  )

  orig_means <- speaker_tracks |>
    dplyr::summarise(
      .by = c(speaker, id),
      dplyr::across(
        F1:F3,
        mean
      )
    ) |>
    tidyr::pivot_longer(
      F1:F3,
      names_to = "formant",
      values_to = "mean"
    )

  idct_ranges <- speaker_idct |>
    dplyr::summarise(
      .by = c(speaker, id),
      dplyr::across(
        F1:F3,
        .fns = list(min = min, max = max)
      )
    ) |>
    tidyr::pivot_longer(
      F1_min:F3_max,
      names_to = c("formant", "metric"),
      names_sep = "_"
    ) |>
    tidyr::pivot_wider(
      names_from = metric,
      values_from = value
    )

  orig_idct_compare <- orig_means |>
    dplyr::left_join(
      idct_ranges
    )

  expect_true(
    all(
      orig_idct_compare$mean > orig_idct_compare$min,
      na.rm = T
    )
  )

  expect_true(
    all(
      orig_idct_compare$mean < orig_idct_compare$max,
      na.rm = T
    )
  )
})
