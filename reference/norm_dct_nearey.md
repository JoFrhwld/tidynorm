# Nearey DCT Normalization

Nearey DCT Normalization

## Usage

``` r
norm_dct_nearey(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .by_formant = FALSE,
  .param_col = NULL,
  .drop_orig = FALSE,
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

- .token_id_col:

  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html)
  A column that identifies token ids.

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to group by. Typically a column of speaker IDs.

- .by_formant:

  Whether or not the normalization method is formant intrinsic.

- .param_col:

  A column identifying the DCT parameter number.

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

A data frame of Nearey normalized DCT coefficients

## Details

**Important**: This function assumes that the DCT coefficients were
estimated over log-transformed formant values.

When formant extrinsic: \$\$ \hat{F}\_{ij} = \log(F\_{ij}) - L \$\$ \$\$
L = \frac{1}{MN}\sum\_{i=1}^M\sum\_{j=1}^N \log(F\_{ij}) \$\$

When formant intrinsic: \$\$ \hat{F}\_{ij} = \log(F\_{ij}) - L\_{i} \$\$

\$\$ L_i = \frac{1}{N}\sum\_{j=1}^{N}\log(F\_{ij}) \$\$

Where

- \\\hat{F}\\ is the normalized formant

- \\i\\ is the formant number

- \\j\\ is the token number

## References

Nearey, T. M. (1978). Phonetic Feature Systems for Vowels \[Ph.D.\].
University of Alberta.

## Examples

``` r
library(tidynorm)
library(dplyr)
ggplot2_inst <- require(ggplot2)

speaker_dct <- speaker_tracks |>
  mutate(
    across(
      F1:F3,
      log
    )
  ) |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )

# Normalize DCT coefficients
speaker_dct_norm <- speaker_dct |>
  norm_dct_nearey(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )
#> Normalization info
#> • normalized with `tidynorm::norm_dct_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_lm`, `F2_lm`, and `F3_lm`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - mean(.formant, na.rm = T))/(1/sqrt(2))
#> 

# Apply average and apply inverse dct
# to plot tracks
track_norm_means <- speaker_dct_norm |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      ends_with("_lm"),
      mean
    )
  ) |>
  reframe_with_idct(
    ends_with("_lm"),
    .by = speaker,
    .token_id_col = vowel,
    .param_col = .param
  )


if (ggplot2_inst) {
  track_norm_means |>
    ggplot(
      aes(F2_lm, F1_lm, color = speaker)
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
