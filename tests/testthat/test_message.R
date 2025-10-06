test_that(
  "verbosity options work", {
    expect_message(
      speaker_data |>
        norm_generic(
          F1:F3,
          .by = speaker
        )
    )

    expect_no_message(
      with_options(
        tidynorm.silent = TRUE,
        speaker_data |>
          norm_generic(
            F1:F3,
            .by = speaker
          )
      )
    )
  }
)
