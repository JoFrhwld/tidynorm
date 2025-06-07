library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_deltaF <- speaker_data |>
  norm_deltaF(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_df"
  )

if (ggplot2_inst) {
  ggplot(
    speaker_data_deltaF,
    aes(
      F2_df,
      F1_df,
      color = speaker
    )
  ) +
    stat_density_2d(
      bins = 4
    ) +
    scale_color_brewer(
      palette = "Dark2"
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    coord_fixed()
}
