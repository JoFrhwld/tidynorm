library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

track_subset <- speaker_tracks |>
  filter(
    .by = c(speaker, id),
    if_all(
      F1:F3,
      .fns = \(x) mean(is.finite(x)) > 0.9
    ),
    row_number() %% 2 == 1
  )

track_norm <- track_subset |>
  norm_track_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .drop_orig = TRUE
  )

if (ggplot2_inst) {
  track_norm |>
    ggplot(
      aes(F2_bz, F1_bz, color = speaker)
    ) +
    stat_density_2d(bins = 4) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}


# returning the DCT coefficients
track_norm_dct <- track_subset |>
  norm_track_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .drop_orig = TRUE,
    .return_dct = TRUE,
    .names = "{.formant}_bz"
  )

track_norm_means <- track_norm_dct |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      ends_with("_bz"),
      mean
    )
  ) |>
  reframe_with_idct(
    ends_with("_bz"),
    .by = speaker,
    .token_id_col = vowel,
    .param_col = .param
  )


if (ggplot2_inst) {
  track_norm_means |>
    ggplot(
      aes(F2_bz, F1_bz, color = speaker)
    ) +
    geom_path(
      aes(
        group = interaction(speaker, vowel)
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}
