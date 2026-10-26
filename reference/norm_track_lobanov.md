# Lobanov Track Normalization

Lobanov Track Normalization

## Usage

``` r
norm_track_lobanov(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .agg_by = NULL,
  .agg_fun = function(x) mean(x, na.rm = T),
  .time_col = NULL,
  .order = 5,
  .return_dct = FALSE,
  .drop_orig = FALSE,
  .names = "{.formant}_z",
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

- .token_id_col:

  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html)
  A column that identifies token ids.

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to group by. Typically a column of speaker IDs.

- .agg_by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to to pre-aggregate by. Typically a column of
  vowel category labels.

- .agg_fun:

  A function to use to pre-aggregate values.

- .time_col:

  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html)
  A time column. (optional)

- .order:

  The number of DCT parameters to use.

- .return_dct:

  Whether or not the normalized DCT coefficients themselves should be
  returned.

- .drop_orig:

  Should the originally targeted columns be dropped.

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

A data frame of Lobanov normalized formant tracks.

## Details

\$\$ \hat{F}\_{ij} = \frac{F\_{ij} - L_i}{S_i} \$\$

\$\$ L_i = \frac{1}{N}\sum\_{j=1}^{N}F\_{ij} \$\$

\$\$ S_i = \sqrt{\frac{\sum(F\_{ij}-L_i)^2}{N-1}} \$\$

Where

- \\\hat{F}\\ is the normalized formant

- \\i\\ is the formant number

- \\j\\ is the token number

### Pre-aggregation

To address imbalanced data across vowel classes, you can pre-aggregate
the zero^(th) DCT coefficients by passing vowel class column(s) to
`.agg_by`, and and an aggregating function to `.agg_fun`. The
aggregation will be done after the DCT transform is applied, and will be
done either formant intrinsically or formant extrinsically based on the
setting passed to `.by_formant`.

## References

Lobanov, B. (1971). Classification of Russian vowels spoken by different
listeners. Journal of the Acoustical Society of America, 49, 606–608.

## Examples

``` r
library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

track_subset <- speaker_tracks |>
  filter(
    .by = c(speaker, id),
    if_all(
      F1:F3,
      .fns = \(x) mean(is.finite(x)) > 0.9
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
#> Normalization info
#> • normalized with `tidynorm::norm_track_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/sd(.formant, na.rm = T)
#> 

if (ggplot2_inst) {
  track_norm |>
    ggplot(
      aes(F2_z, F1_z, color = speaker)
    ) +
    stat_density_2d(bins = 4) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}


# returning the DCT coefficients
track_norm_dct <- track_subset |>
  norm_track_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .return_dct = TRUE,
    .drop_orig = TRUE,
    .names = "{.formant}_z"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/sd(.formant, na.rm = T)
#> 

track_norm_means <- track_norm_dct |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      ends_with("_z"),
      mean
    )
  ) |>
  reframe_with_idct(
    ends_with("_z"),
    .by = speaker,
    .token_id_col = vowel,
    .param_col = .param
  )


if (ggplot2_inst) {
  track_norm_means |>
    ggplot(
      aes(F2_z, F1_z, color = speaker)
    ) +
    geom_path(
      aes(
        group = interaction(speaker, vowel)
      )
    ) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}
```
