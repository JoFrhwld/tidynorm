# Delta F Track Normalization

Delta F Track Normalization

## Usage

``` r
norm_track_deltaF(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .time_col = NULL,
  .order = 5,
  .return_dct = FALSE,
  .drop_orig = FALSE,
  .names = "{.formant}_df",
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

A data frame of Delta F normalized formant tracks.

## Details

\$\$ \hat{F}\_{ij} = \frac{F\_{ij}}{S} \$\$ \$\$ S =
\frac{1}{MN}\sum\_{i=1}^M\sum\_{j=1}^N \frac{F\_{ij}}{i-0.5} \$\$

Where

- \\\hat{F}\\ is the normalized formant

- \\i\\ is the formant number

- \\j\\ is the token number

## References

Johnson, K. (2020). The \\\Delta\\F method of vocal tract length
normalization for vowels. Laboratory Phonology: Journal of the
Association for Laboratory Phonology, 11(1), Article 1.
[doi:10.5334/labphon.196](https://doi.org/10.5334/labphon.196)

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
  norm_track_deltaF(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .drop_orig = TRUE
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_deltaF()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_df`, `F2_df`, and `F3_df`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - 0)/mean(.formant/(.formant_num - 0.5), na.rm = T)
#> 

if (ggplot2_inst) {
  track_norm |>
    ggplot(
      aes(F2_df, F1_df, color = speaker)
    ) +
    stat_density_2d(bins = 4) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}



# returning the DCT coefficients
track_norm_dct <- track_subset |>
  norm_track_deltaF(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t,
    .drop_orig = TRUE,
    .return_dct = TRUE,
    .names = "{.formant}_df"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_deltaF()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_df`, `F2_df`, and `F3_df`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - 0)/mean(.formant/(.formant_num - 0.5), na.rm = T)
#> 

track_norm_means <- track_norm_dct |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      ends_with("_df"),
      mean
    )
  ) |>
  reframe_with_idct(
    ends_with("_df"),
    .by = speaker,
    .token_id_col = vowel,
    .param_col = .param
  )


if (ggplot2_inst) {
  track_norm_means |>
    ggplot(
      aes(F2_df, F1_df, color = speaker)
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
