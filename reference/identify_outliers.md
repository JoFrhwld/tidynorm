# Identify Outliers

Identify Outliers

## Usage

``` r
identify_outliers(
  .data,
  ...,
  .by = NULL,
  .pre_trans = identity,
  .level = 0.99,
  .call = caller_env(),
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

- .pre_trans:

  A function to apply to formant values before normalization.

- .level:

  Probability level above which tokens are labelled as outliers.

- .call:

  Used for internal purposes.

- .silent:

  Suppress normalization information messages when running a `norm_*()`
  function. (Defaults to `FALSE`, overwritable using option
  'tidynorm.silent' or environment variable
  'R_TIDYNORM_TIDYNORM_SILENT')

## Value

The same data frame as input, with three additional columns.

- .mahal:

  The squared Malahanobis Distance
  ([stats::mahalanobis](https://rdrr.io/r/stats/mahalanobis.html))

- .prob:

  The the probability of `.mahal` according to the chi-squared
  distribution with degrees of freedom equal to the number of data
  dimensions

- .outlier:

  Whether or not `.prob` is greater than the probability level defined
  by .level

## Details

This function will identify outliers using the Mahalanobis distance
without removing them.

## Examples

``` r
library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

# Identify outliers in F1 x F2 space

speaker_data |>
  identify_outliers(
    F1:F2,
    .by = c(speaker, plt_vclass),
    .level = 0.95
  ) ->
  outliers_marked
#> Normalization info
#> • outliers identified along `F1` and `F2`
#> • grouped by `speaker` and `plt_vclass`
#> • outlier probability level: 0.95
#> 

outliers_marked |>
  count(.outlier)
#> # A tibble: 2 × 2
#>   .outlier     n
#>   <lgl>    <int>
#> 1 FALSE    10107
#> 2 TRUE       590

if (ggplot2_inst) {
  outliers_marked |>
    filter(
      plt_vclass == "ae"
    ) |>
    ggplot(
      aes(F2, F1, color = .outlier)
    ) +
    geom_point() +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}


if (ggplot2_inst) {
  outliers_marked |>
    filter(
      plt_vclass == "ae"
    ) |>
    ggplot(
      aes(F2, F1, color = sqrt(.mahal))
    ) +
    geom_point() +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}
```
