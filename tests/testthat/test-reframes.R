test_that(
  "reframe with idct smooth returns correct lengths",
  {
    speaker_tracks |>
      dplyr::mutate(
        .by = c(speaker, id),
        rowid = dplyr::row_number()
      ) |>
      dplyr::filter(
        (id %% 2 == 0 & rowid %% 2 == 1) |
          id %% 2 == 1
      ) ->
      diff_size

    diff_size |>
      reframe_with_dct_smooth(
        F1:F3,
        .time_col = t,
        .token_id_col = id,
        .by = speaker
      ) ->
      diff_size_smooths

    diff_size |>
      dplyr::count(speaker, id) |>
      dplyr::rename(orig_n = n) ->
      orig_count

    diff_size_smooths |>
      dplyr::count(speaker, id) |>
      dplyr::rename(smooth_n = n) ->
      smooth_count

    orig_count |>
      dplyr::left_join(smooth_count)->
      count_comp

    expect_equal(count_comp$orig_n, count_comp$smooth_n, ignore_attr = TRUE)

  }
)
