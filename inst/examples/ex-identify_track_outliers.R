library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

speaker_tracks |>
  identify_track_outliers(
    F1:F2,
    .token_id_col = id,
    .by = c(speaker, plt_vclass),
    .level = 0.9
  ) ->
  outliers_marked

speaker_tracks |>
  identify_track_outliers(
    F1:F2,
    .token_id_col = id,
    .by = c(speaker, plt_vclass),
    .level = 0.9,
    .return_dct = TRUE
  ) ->
  outliers_dct_marked

if (ggplot2_inst) {
  outliers_marked |>
    filter(plt_vclass == "e") |>
    ggplot(
      aes(F2, F1, color = .outlier)
    ) +
    geom_path(
      aes(group = id),
      arrow = arrow(
        type = "closed",
        length = unit(0.2, "cm")
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}

if (ggplot2_inst) {
  outliers_marked |>
    filter(plt_vclass == "e") |>
    ggplot(
      aes(F2, F1, color = sqrt(.mahal))
    ) +
    geom_path(
      aes(group = id),
      arrow = arrow(
        type = "closed",
        length = unit(0.2, "cm")
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}

if (ggplot2_inst) {
  outliers_dct_marked |>
    filter(plt_vclass == "e") |>
    reframe_with_idct(
      F1:F3,
      .token_id_col = id,
      .by = speaker
    ) |>
    ggplot(
      aes(F2, F1, color = .outlier)
    ) +
    geom_path(
      aes(group = id),
      arrow = arrow(
        type = "closed",
        length = unit(0.2, "cm")
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}
