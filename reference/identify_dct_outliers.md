# Identify DCT Outliers

Identify DCT Outliers

## Usage

``` r
identify_dct_outliers(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .param_col = NULL,
  .level = 0.99,
  .call = caller_env(),
  .silent = opt("tidynorm.silent")
)
```

## Arguments

- .data:

  A data frame of formant DCT coefficients

- ...:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  One or more unquoted expressions separated by commas. These should
  target the vowel formant data columns.

- .token_id_col:

  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html)
  A column that identifies token ids.

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to group by. Typically a column of speaker IDs.

- .param_col:

  A column identifying the DCT parameter number.

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

## Examples

``` r
library(tidynorm)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
ggplot2_inst <- require(ggplot2)
#> Loading required package: ggplot2

speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .token_id_col = id,
    .by = speaker,
    .time_col = t
  ) ->
  speaker_dct

speaker_dct |>
  identify_dct_outliers(
    F1:F2,
    .by = c(speaker, plt_vclass),
    .token_id_col = id,
    .param_col = .param,
    .level = 0.9
  ) ->
  outliers_marked
#> Normalization info
#> • outliers identified along `F1` and `F2`
#> • grouped by `speaker` and `plt_vclass`
#> • outlier probability level: 0.9
#> 


if (ggplot2_inst) {
  outliers_marked |>
    filter(plt_vclass == "e") |>
    reframe_with_idct(
      F1:F3,
      .token_id_col = id,
      .param_col = .param,
      .by = speaker
    ) |>
    ggplot(
      aes(F2, F1, color = .outlier)
    ) +
    geom_path(
      aes(group = id),
      arrow = arrow(
        type = "closed",
        length = unit(0.2, "cm")
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}



if (ggplot2_inst) {
  outliers_marked |>
    filter(plt_vclass == "e") |>
    reframe_with_idct(
      F1:F3,
      .token_id_col = id,
      .param_col = .param,
      .by = speaker
    ) |>
    ggplot(
      aes(F2, F1, color = sqrt(.mahal))
    ) +
    geom_path(
      aes(group = id),
      arrow = arrow(
        type = "closed",
        length = unit(0.2, "cm")
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    facet_wrap(~speaker)
}
```
