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

track_dcts <- track_subset |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .order = 3
  )

track_norm <- track_dcts |>
  norm_dct_generic(
    F1:F3,
    .token_id_col = id,
    .by = speaker,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = TRUE),
    .S = mad(.formant, na.rm = TRUE),
    .param_col = .param,
    .drop_orig = TRUE,
    .names = "{.formant}_mad"
  )

head(track_norm)

full_tracks <- track_norm |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      F1_mad:F3_mad,
      mean
    )
  ) |>
  reframe_with_idct(
    F1_mad:F3_mad,
    .by = c(speaker, vowel),
    .param_col = .param
  )

head(full_tracks)

if (ggplot2_inst) {
  ggplot(
    full_tracks,
    aes(F2_mad, F1_mad, color = speaker)
  ) +
    geom_path(
      aes(group = interaction(speaker, vowel))
    ) +
    scale_y_reverse() +
    scale_x_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}
