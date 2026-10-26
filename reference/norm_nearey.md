# Nearey Normalize

Nearey Normalize

## Usage

``` r
norm_nearey(
  .data,
  ...,
  .by = NULL,
  .by_formant = FALSE,
  .agg_by = NULL,
  .agg_fun = function(x) mean(x, na.rm = T),
  .drop_orig = FALSE,
  .keep_params = FALSE,
  .names = "{.formant}_lm",
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

  Whether or not the normalization method is formant intrinsic.

- .agg_by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to to pre-aggregate by. Typically a column of
  vowel category labels.

- .agg_fun:

  A function to use to pre-aggregate values.

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

A data frame of Nearey normalized formant values.

## Details

When formant extrinsic: \$\$ \hat{F}\_{ij} = \log(F\_{ij}) - L \$\$ \$\$
L = \frac{1}{MN}\sum\_{i=1}^M\sum\_{j=1}^N \log(F\_{ij}) \$\$

When formant intrinsic: \$\$ \hat{F}\_{ij} = \log(F\_{ij}) - L\_{i} \$\$

\$\$ L_i = \frac{1}{N}\sum\_{j=1}^{N}\log(F\_{ij}) \$\$

Where

- \\\hat{F}\\ is the normalized formant

- \\i\\ is the formant number

- \\j\\ is the token number

### Pre-aggregation

To address imbalanced data across vowel classes, you can pre-aggregate
formant values by passing vowel class column(s) to `.agg_by`, and and an
aggregating function to `.agg_fun`. The aggregation will be done after
any `.pre_trans` transformation is applied, and will be done either
formant intrinsically or formant extrinsically based on the setting
passed to `.by_formant`.

## References

Nearey, T. M. (1978). Phonetic Feature Systems for Vowels \[Ph.D.\].
University of Alberta.

## Examples

``` r
library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_nearey <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .by_formant = FALSE,
    .names = "{.formant}_nearey"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_nearey`, `F2_nearey`, and `F3_nearey`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • Transformation prior to normalization: log
#> • (.formant - mean(.formant, na.rm = T))/(1)
#> 

if (ggplot2_inst) {
  ggplot(
    speaker_data_nearey,
    aes(
      F2_nearey,
      F1_nearey,
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
    coord_fixed() +
    labs(
      title = "Formant extrinsic"
    )
}


speaker_data_nearey2 <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .by_formant = TRUE,
    .names = "{.formant}_nearey"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_nearey`, `F2_nearey`, and `F3_nearey`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • Transformation prior to normalization: log
#> • (.formant - mean(.formant, na.rm = T))/(1)
#> 

if (ggplot2_inst) {
  ggplot(
    speaker_data_nearey2,
    aes(
      F2_nearey,
      F1_nearey,
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
    coord_fixed() +
    labs(
      title = "Formant intrinsic"
    )
}
```
