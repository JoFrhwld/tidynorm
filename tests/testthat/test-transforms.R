test_that("original formants aren't transformed",{
  orig_f1 <- speaker_data$F1
  log_f1 <- log(speaker_data$F1)

  lnorm <- speaker_data |>
    norm_generic(
      F1:F3,
      .by = speaker,
      .pre_trans = log,
      .silent = T,
      .names = "{.formant}_n"
    )

  expect_equal(orig_f1, lnorm$F1, ignore_attr = TRUE)
  expect_equal(log_f1, lnorm$F1_n, ignore_attr = TRUE)
}
)
