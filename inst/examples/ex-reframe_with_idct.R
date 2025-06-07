library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

speaker_small <- filter(
  speaker_tracks,
  id == 0
)

speaker_dct <- speaker_small |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .order = 5
  )

speaker_idct <- speaker_dct |>
  reframe_with_idct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param,
    .n = 20
  )

if (ggplot2_inst) {
  speaker_small |>
    mutate(
      .by = c(speaker, id),
      time_index = row_number()
    ) |>
    ggplot(
      aes(
        time_index, F1
      )
    ) +
    geom_point() +
    labs(
      title = "Original Data"
    )
}

if (ggplot2_inst) {
  speaker_idct |>
    ggplot(
      aes(
        .time, F1
      )
    ) +
    geom_point() +
    labs(
      title = "DCT Smooth Data"
    )
}
