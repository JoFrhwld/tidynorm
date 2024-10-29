library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_barkz <- speaker_data |>
  norm_barkz(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_bz"
  )

## this is equivalent to
# speaker_data |>
#   norm_generic(
#     F1:F3,
#     .by = speaker,
#     .by_token = T,
#     .L = .formant[3]
#   )

if(ggplot2_inst){
  ggplot(
    speaker_data_barkz,
    aes(
      F2_bz,
      F1_bz,
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

