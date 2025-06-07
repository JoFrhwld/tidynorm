library(tidynorm)
library(dplyr)

ggplot2_inst <- require(ggplot2)

speaker_small <- filter(
  speaker_tracks,
  id == 0
)

speaker_dct_smooth <- speaker_small |>
  reframe_with_dct_smooth(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .order = 5
  )

if (ggplot2_inst) {
  speaker_small |>
    ggplot(
      aes(
        t, F1
      )
    ) +
    geom_point() +
    facet_wrap(
      ~speaker,
      scales = "free_x",
      ncol = 1
    ) +
    labs(
      title = "Original Data"
    )
}

if (ggplot2_inst) {
  speaker_dct_smooth |>
    ggplot(
      aes(
        t, F1
      )
    ) +
    geom_point() +
    facet_wrap(
      ~speaker,
      scales = "free_x",
      ncol = 1
    ) +
    labs(
      title = "Smoothed Data"
    )
}
