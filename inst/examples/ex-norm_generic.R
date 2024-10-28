library(tidynorm)
library(dplyr)

speaker_data |>
  norm_generic(
    F1:F3,
    .by = speaker,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = T),
    .S = mad(.formant, na.rm = T),
    .drop_orig = TRUE,
    .names = "{.formant}_mad"
  )

