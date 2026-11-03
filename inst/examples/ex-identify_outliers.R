library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

# Identify outliers in F1 x F2 space

speaker_data |>
  identify_outliers(
    F1:F2,
    .by = c(speaker, plt_vclass),
    .level = 0.95
  ) ->
  outliers_marked

outliers_marked |>
  count(.outlier)

if (ggplot2_inst) {
  outliers_marked |>
    filter(
      plt_vclass == "ae"
    ) |>
    ggplot(
      aes(F2, F1, color = .outlier)
    ) +
    geom_point() +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}

if (ggplot2_inst) {
  outliers_marked |>
    filter(
      plt_vclass == "ae"
    ) |>
    ggplot(
      aes(F2, F1, color = sqrt(.mahal))
    ) +
    geom_point() +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}
