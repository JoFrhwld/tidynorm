test_that(
  "option setting works",
  {
    tidynorm_options(.silent = TRUE, .warnings = FALSE)
    expect_no_message({
      speaker_data |> norm_generic(F1:F3)
    })

    expect_no_warning({
      speaker_data |> norm_generic(F1:F3)
    })

    tidynorm_options(.silent = FALSE, .warnings = TRUE)
    expect_message({
      speaker_data |> norm_generic(F1:F3, .by = speaker)
    })
    expect_warning({
      speaker_data |> norm_generic(F1:F3)
    })
  }
)
