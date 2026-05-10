# Watt & Fabricius Normalize

Watt & Fabricius Normalize

## Usage

``` r
norm_wattfab(
  .data,
  ...,
  .by = NULL,
  .by_formant = TRUE,
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.formant}_wf",
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

- .by_formant:

  Ignored by this procedure

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

A data fame of Watt & Fabricius normalized formant values.

## Details

This is a modified version of the Watt & Fabricius Method. The original
method identified point vowels over which F1 and F2 centroids were
calculated. The procedure here just identifies centroids by taking the
mean of all formant values.

\$\$ \hat{F}\_{ij} = \frac{F\_{ij}}{S_i} \$\$

\$\$ S_i = \frac{1}{N}\sum\_{j=1}^N F\_{ij} \$\$

Where

- \\\hat{F}\\ is the normalized formant

- \\i\\ is the formant number

- \\j\\ is the token number

## References

Watt, D., & Fabricius, A. (2002). Evaluation of a technique for
improving the mapping of multiple speakers’ vowel spaces in the F1 ~ F2
plane. Leeds Working Papers in Linguistics and Phonetics, 9, 159–173.

## Examples

``` r
library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_wattfab <- speaker_data |>
  norm_wattfab(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_wf"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_wattfab()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_wf`, `F2_wf`, and `F3_wf`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - 0)/(mean(.formant, na.rm = T))
#> 

if (ggplot2_inst) {
  ggplot(
    speaker_data_wattfab,
    aes(
      F2_wf,
      F1_wf,
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
```
