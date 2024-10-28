library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_lobanov <- speaker_data |>
  norm_lobanov(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_z"
  )

## this is equivalent to
# speaker_data |>
#   norm_generic(
#     F1:F3,
#     .by = speaker,
#     .by_formant = T,
#     .L = mean(.formant, na.rm = T),
#     .S = sd(.formant, na.rm = T)
#   )

if(ggplot2_inst){
  ggplot(
    speaker_data_lobanov,
    aes(
      F2_z,
      F1_z,
      color = speaker
    )
  )+
    stat_density_2d(
      bins = 4
    )+
    scale_color_brewer(
      palette = "Dark2"
    )+
    scale_x_reverse()+
    scale_y_reverse()+
    coord_fixed()
}
