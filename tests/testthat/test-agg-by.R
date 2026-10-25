test_that(
  "pre-aggregation returns the same results as manual approach", {
    speaker_data |>
      dplyr::mutate(id = row_number()) |>
      tidyr::pivot_longer(
        F1:F3,
        names_to = ".formant_name",
        values_to = ".formant"
      ) -> slong

      slong |>
        dplyr::summarise(
        .by = c(speaker, plt_vclass, .formant_name),
        .formant = mean(.formant, na.rm = T)
      ) |>
        dplyr::summarise(
        .by = c(speaker, .formant_name),
        .L = mean(.formant, na.rm = T),
        .S = sd(.formant, na.rm = T)
      ) ->
        norm_param

      slong |>
        dplyr::left_join(norm_param) |>
        dplyr::mutate(
          norm = (.formant - .L)/.S,
          .formant_name = stringr::str_glue("{.formant_name}_n1")
        ) |>
        dplyr::select(
          speaker:.formant_name, norm
        ) |>
        tidyr::pivot_wider(
          names_from = ".formant_name",
          values_from = "norm"
        ) |>
        dplyr::select(id, F1_n1, F2_n1, F3_n1) ->
        manual_norm

      speaker_data |>
        dplyr::mutate(
          id = dplyr::row_number()
        ) |>
        norm_generic(
          F1:F3,
          .by = speaker,
          .agg_by = plt_vclass,
          .agg_fun = \(x) mean(x, na.rm = T),
          .by_formant = T,
          .L = mean(.formant, na.rm = T),
          .S = sd(.formant, na.rm = T),
          .silent = T
        ) ->
        tidy_norm

      tidy_norm |>
        dplyr::left_join(
          manual_norm,
          by = c("id")
        ) ->
        comp_norm

      expect_equal(
        comp_norm$F1_n,
        comp_norm$F1_n1,
        ignore_attr = TRUE
      )

      expect_equal(
        comp_norm$F2_n,
        comp_norm$F2_n1,
        ignore_attr = TRUE
      )

      expect_equal(
        comp_norm$F3_n,
        comp_norm$F3_n1,
        ignore_attr = TRUE
      )

   }
)

test_that(
  "Various grouping has no problems", {
    expect_no_warning(
      speaker_data |>
        norm_generic(
          F1:F3,
          .by = speaker,
          .agg_by = plt_vclass,
          .agg_fun = \(x) mean(x, na.rm = T),
          .by_formant = T,
          .L = mean(.formant, na.rm = T),
          .S = sd(.formant, na.rm = T),
          .silent = T
        )
    )

    expect_no_warning(
      speaker_data |>
        norm_generic(
          F1:F3,
          .by = speaker,
          .agg_by = c(plt_vclass, vowel),
          .agg_fun = \(x) mean(x, na.rm = T),
          .by_formant = T,
          .L = mean(.formant, na.rm = T),
          .S = sd(.formant, na.rm = T),
          .silent = T
        )
    )

    expect_no_warning(
      speaker_data |>
        group_by(speaker) |>
        norm_generic(
          F1:F3,
          .agg_by = c(plt_vclass, vowel),
          .agg_fun = \(x) mean(x, na.rm = T),
          .by_formant = T,
          .L = mean(.formant, na.rm = T),
          .S = sd(.formant, na.rm = T),
          .silent = T
        )
    )
  }
)

