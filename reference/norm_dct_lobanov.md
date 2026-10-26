# Lobanov DCT Normalization

Lobanov DCT Normalization

## Usage

``` r
norm_dct_lobanov(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .agg_by = NULL,
  .agg_fun = function(x) mean(x, na.rm = T),
  .param_col = NULL,
  .names = "{.formant}_z",
  .silent = opt("tidynorm.silent"),
  .drop_orig = FALSE
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

- .agg_by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to to pre-aggregate by. Typically a column of
  vowel category labels.

- .agg_fun:

  A function to use to pre-aggregate values.

- .param_col:

  A column identifying the DCT parameter number.

- .names:

  A [`glue::glue()`](https://glue.tidyverse.org/reference/glue.html)
  expression for naming the normalized data columns. The `"{.formant}"`
  portion corresponds to the name of the original formant columns.

- .silent:

  Suppress normalization information messages when running a `norm_*()`
  function. (Defaults to `FALSE`, overwritable using option
  'tidynorm.silent' or environment variable
  'R_TIDYNORM_TIDYNORM_SILENT')

- .drop_orig:

  Should the originally targeted columns be dropped.

## Value

A data frame of Lobanov normalized DCT Coefficients.

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

speaker_dct <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )

# Normalize DCT coefficients
speaker_dct_norm <- speaker_dct |>
  norm_dct_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )
#> Normalization info
#> • normalized with `tidynorm::norm_dct_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/sd(.formant, na.rm = T)
#> 

# Apply average and apply inverse dct
# to plot tracks
track_norm_means <- speaker_dct_norm |>
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
