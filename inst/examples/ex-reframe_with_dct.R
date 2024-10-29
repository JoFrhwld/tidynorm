library(tidynorm)
library(dplyr)

speaker_small <- filter(
  speaker_tracks,
  id == 0
)

speaker_dct <- reframe_with_dct(
  speaker_small,
  F1:F3,
  .by = speaker,
  .token_id_col = id,
  .time_col = t
)

head(
  speaker_dct
)
