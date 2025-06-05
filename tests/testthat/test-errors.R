test_that("grouping managed", {
  # group_by() |> norm_ should error
  expect_error({
    speaker_data |>
      dplyr::group_by(speaker) |>
      norm_generic(
        F1:F3,
        .by = speaker
      )
  })

  expect_no_error({
    speaker_data |>
      dplyr::group_by(speaker) |>
      norm_nearey(
        F1:F3,
        .by_formant = T,
        .silent = T
      )
  })
})
