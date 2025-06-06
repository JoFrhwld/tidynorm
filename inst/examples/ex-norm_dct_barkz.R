library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

speaker_dct <- speaker_tracks |>
  mutate(
    across(F1:F3, hz_to_bark)
  ) |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )

# Normalize DCT coefficients
speaker_dct_norm <- speaker_dct |>
  norm_dct_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )

# Apply average and apply inverse dct
# to plot tracks
track_norm_means <- speaker_dct_norm |>
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
