test_that(
  "formants are correctly ordered in normalization",
  {
    # Subtract F3 from all formants
    f3_minus <- speaker_data |>
      norm_generic(
        # pass targets in reverse order
        F3:F1,
        .by = speaker,
        .by_token = TRUE,
        .by_formant = FALSE,
        .L = .formant[3],
        .names = "{.formant}_n",
        .silent = T
      )

    # F3 should be zero
    expect_true(
      all(f3_minus$F3_n == 0, na.rm = T)
    )

  }
)


test_that(
  "idiosyncratic formant column names are ok",
  {
    # create some idiosyncratic names
    speaker_data |>
      mutate(
        across(
          F1:F3,
          \(x)x,
          .names = "{.col}_1"
        ),
        across(
          F1:F3,
          \(x)x,
          .names = "{tolower(.col)}"
        ),
        F_one = F1,
        F_two = F2,
        F_three = F3,
        first_formant = F1,
        second_formant = F2,
        third_formant = F3
        ) ->
      weird_names

    # test 1
    norm1 <- weird_names |>
      norm_generic(
        F1_1:F3_1,
        .by = speaker,
        .by_token = TRUE,
        .by_formant = FALSE,
        .L = .formant[3],
        .names = "{.formant}_n",
        .silent = T
      )

    expect_true(
      all(norm1$F3_1_n == 0, na.rm = T)
    )

    # test 2
    norm2 <- weird_names |>
      norm_generic(
        F_one:F_three,
        .by = speaker,
        .by_token = TRUE,
        .by_formant = FALSE,
        .L = .formant[3],
        .names = "{.formant}_n",
        .silent = T
      )

    expect_true(
      all(norm2$F_three_n == 0, na.rm = T)
    )


    # test 3
    norm3 <- weird_names |>
      norm_generic(
        first_formant:third_formant,
        .by = speaker,
        .by_token = TRUE,
        .by_formant = FALSE,
        .L = .formant[3],
        .names = "{.formant}_n",
        .silent = T
      )

    expect_true(
      all(norm3$third_formant_n == 0, na.rm = T)
    )

  }
)
