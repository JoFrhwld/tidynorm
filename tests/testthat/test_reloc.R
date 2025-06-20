are_names_aligned <- function(orig_cols, new_cols){

  orig_check <- seq_along(orig_cols) |>
    set_names(orig_cols)
  new_check <- seq_along(new_cols) |>
    set_names(new_cols)

  for (i in seq_along(orig_check)) {
    if (i == 1) {
      expect_equal(
        names(new_check)[i], names(orig_check)[i]
      )
    } else {
      curr_name <- names(orig_check)[i]
      prev_name <- names(orig_check)[i - 1]

      expect_gt(
        new_check[curr_name], new_check[prev_name]
      )
    }
  }
}


test_that(
  "point norm output columns are correctly ordered",
  {
    orig_cols <- colnames(speaker_data)
    norm_data <- speaker_data |> norm_generic(F1:F3, .by = speaker, .silent = T)
    new_cols <- colnames(norm_data)
    are_names_aligned(orig_cols, new_cols)

    formant_start <- speaker_data |>
      dplyr::relocate(
        F1:F3,
        .before = 1
      )

    orig_cols <- colnames(formant_start)
    norm_data <- formant_start |> norm_generic(F1:F3, .by = speaker, .silent = T)
    new_cols <- colnames(norm_data)
    are_names_aligned(orig_cols, new_cols)

  }
)


test_that(
  "track norm output columns are correctly ordered",
  {
    orig_cols <- colnames(speaker_tracks)
    # with time col
    norm_data <- speaker_tracks |> norm_track_generic(
      F1:F2,
      .by = speaker,
      .token_id_col = id,
      .time_col = t,
      .silent = TRUE
      )
    new_cols <- colnames(norm_data)
    are_names_aligned(orig_cols, new_cols)


    # without time col
    norm_data <- speaker_tracks |> norm_track_generic(
      F1:F2,
      .by = speaker,
      .token_id_col = id,
      .silent = TRUE
    )
    new_cols <- colnames(norm_data)
    are_names_aligned(orig_cols, new_cols)

    # formant first with time col
    formant_first <- speaker_tracks |>
      relocate(
        F1:F3, .before = 1
      )

    orig_cols <- colnames(formant_first)
    norm_data <- formant_first |> norm_track_generic(
      F1:F2,
      .by = speaker,
      .token_id_col = id,
      .time_col = t,
      .silent = TRUE
    )
    new_cols <- colnames(norm_data)
    are_names_aligned(orig_cols, new_cols)

    # formant first without time col
    norm_data <- formant_first |> norm_track_generic(
      F1:F2,
      .by = speaker,
      .token_id_col = id,
      .silent = TRUE
    )
    new_cols <- colnames(norm_data)
    are_names_aligned(orig_cols, new_cols)

  }


)
