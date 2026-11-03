library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .token_id_col = id,
    .by = speaker,
    .time_col = t
  ) ->
  speaker_dct

speaker_dct |>
  identify_dct_outliers(
    F1:F2,
    .by = c(speaker, plt_vclass),
    .token_id_col = id,
    .param_col = .param,
    .level = 0.9
  ) ->
  outliers_marked


if (ggplot2_inst) {
  outliers_marked |>
    filter(plt_vclass == "e") |>
    reframe_with_idct(
      F1:F3,
      .token_id_col = id,
      .param_col = .param,
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


if (ggplot2_inst) {
  outliers_marked |>
    filter(plt_vclass == "e") |>
    reframe_with_idct(
      F1:F3,
      .token_id_col = id,
      .param_col = .param,
      .by = speaker
    ) |>
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
