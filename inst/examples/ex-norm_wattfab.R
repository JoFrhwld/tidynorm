library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_wattfab <- speaker_data |>
  norm_wattfab(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_wf"
  )

if (ggplot2_inst) {
  ggplot(
    speaker_data_wattfab,
    aes(
      F2_wf,
      F1_wf,
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
