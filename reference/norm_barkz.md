# Bark Difference Normalize

Bark Difference Normalize

## Usage

``` r
norm_barkz(
  .data,
  ...,
  .by = NULL,
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.formant}_bz",
  .silent = opt("tidynorm.silent")
)
```

## Arguments

- .data:

  A data frame containing vowel formant data

- ...:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  One or more unquoted expressions separated by commas. These should
  target the vowel formant data columns.

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to group by. Typically a column of speaker IDs.

- .drop_orig:

  Whether or not to drop the original formant data columns.

- .keep_params:

  Whether or not to keep the Location (`*_.L`) and Scale (`*_.S`)
  normalization parameters

- .names:

  A [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  expression for naming the normalized data columns. The `"{.formant}"`
  portion corresponds to the name of the original formant columns.

- .silent:

  Suppress normalization information messages when running a `norm_*()`
  function. (Defaults to `FALSE`, overwritable using option
  'tidynorm.silent' or environment variable
  'R_TIDYNORM_TIDYNORM_SILENT')

## Value

A data frame of Bark Difference normalized formant values

## Details

This is a within-token normalization technique. First all formants are
converted to Bark (see
[hz_to_bark](https://jofrhwld.github.io/tidynorm/reference/hz_to_bark.md)),
then, within each token, F3 is subtracted from F1 and F2.

\$\$ \hat{F}\_{ij} = F\_{ij} - L_j \$\$

\$\$ L_j = F\_{3j} \$\$

## References

Syrdal, A. K., & Gopal, H. S. (1986). A perceptual model of vowel
recognition based on the auditory representation of American English
vowels. The Journal of the Acoustical Society of America, 79(4),
1086–1100. [doi:10.1121/1.393381](https://doi.org/10.1121/1.393381)

## Examples

``` r
library(tidynorm)
ggplot2_inst <- require(ggplot2)
#> Loading required package: ggplot2

speaker_data_barkz <- speaker_data |>
  norm_barkz(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_bz"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_barkz()`
#> • normalized `F1`, `F2`, and `F3`
#> • `F3` used for third formant.
#> • normalized values in `F1_bz`, `F2_bz`, and `F3_bz`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • within token: TRUE
#> • Transformation prior to normalization: hz_to_bark
#> • (.formant - .formant[3])/(1)
#> 

if (ggplot2_inst) {
  ggplot(
    speaker_data_barkz,
    aes(
      F2_bz,
      F1_bz,
      color = speaker
    )
  ) +
    stat_density_2d(
      bins = 4
    ) +
    scale_color_brewer(
      palette = "Dark2"
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    coord_fixed()
}
#> Warning: Removed 42 rows containing non-finite outside the scale range
#> (`stat_density2d()`).
```
