test_that(
  "point barkz errors on no f3", {
    expect_error(
      speaker_data |> norm_barkz(F1:F2, .by = speaker)
    )

    expect_no_error(
      speaker_data |> norm_barkz(F1:F3, .by = speaker, .silent = T)
    )

    expect_error(
      speaker_data |> norm_barkz(c(F1, F3), .by = speaker, .silent = T)
    )
  }
)


test_that(
  "track barkz errors on no f3", {
    expect_error(
      speaker_tracks |> norm_track_barkz(
        F1:F2,
        .by = speaker,
        .token_id_col = id,
        .time_col = t
      )
    )

    expect_no_error(
      speaker_tracks |> norm_track_barkz(
        F1:F3,
        .by = speaker,
        .token_id_col = id,
        .silent = TRUE,
        .time_col = t
      )
    )

    expect_error(
      speaker_tracks |> norm_track_barkz(
        c(F1, F3),
        .by = speaker,
        .token_id_col = id,
        .silent = TRUE,
        .time_col = t
      )
    )
  }
)
