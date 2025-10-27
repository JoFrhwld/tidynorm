valid_infos <- c(
  ".step", ".norm_procedure", ".targets", ".f3", ".norm_cols",
  ".token_id_col", ".time_col", ".param_col", ".by", ".by_formant",
  ".by_token", ".pre_trans", ".norm", ".post_trans"
)

test_that(
  "Test that norminfo is stored correctly",
  {
    speaker_data |>
      norm_generic(
        F1:F3,
        .by = speaker,
        .L = mean(.formant, na.rm = T),
        .pre_trans = log,
        .post_trans = identity,
        .silent = T
      ) ->
    data

    expected_infos <- c(
      ".by", ".norm", ".pre_trans", ".post_trans",
      ".targets", ".norm_cols", ".norm_procedure",
      ".by_formant"
    )

    last_norm <- attr(data, "norminfo") |>
      dplyr::last()

    expect_in(
      names(last_norm),
      valid_infos
    )

    expect_in(
      expected_infos,
      names(last_norm)
    )

  }
)
