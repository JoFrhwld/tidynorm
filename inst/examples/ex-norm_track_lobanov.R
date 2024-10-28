library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

track_subset <- speaker_tracks |>
  filter(
    .by = c(speaker, id),
    if_all(
      F1:F3,
      .fns =\(x) mean(is.finite(x)) > 0.9
    ),
    row_number() %% 2 == 1
  )

track_norm <- track_subset |>
  norm_track_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .drop_orig = TRUE
  )

if(ggplot2_inst){
  track_norm |>
    ggplot(
      aes(F2_z, F1_z, color = speaker)
    )+
    stat_density_2d(bins = 4)+
    scale_x_reverse()+
    scale_y_reverse()+
    scale_color_brewer(palette = "Dark2")+
    coord_fixed()
}
