# Generic Formant Track Normalization Procedure

Normalize formant tracks using Discrete Cosine Transform normalization

## Usage

``` r
norm_track_generic(
  .data,
  ...,
  .token_id_col,
  .by = NULL,
  .by_formant = FALSE,
  .by_token = FALSE,
  .time_col = NULL,
  .L = 0,
  .S = 1/sqrt(2),
  .pre_trans = identity,
  .post_trans = identity,
  .order = 5,
  .return_dct = FALSE,
  .drop_orig = FALSE,
  .names = "{.formant}_n",
  .silent = opt("tidynorm.silent"),
  .call = caller_env()
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

- .by_token:

  Whether or not the normalization method is token intrinsic

- .time_col:

  [`<data-masking>`](https://rlang.r-lib.org/reference/args_data_masking.html)
  A time column. (optional)

- .L:

  An expression defining the location parameter. See Details for more
  information.

- .S:

  An expression defining the scale parameter. See Details for more
  information.

- .pre_trans:

  A function to apply to formant values before normalization.

- .post_trans:

  A function to apply to formant values after normalization.

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

- .call:

  Used for internal purposes.

## Value

A data frame of normalized formant tracks.

## Details

The following `norm_track_*` procedures were built on top of
`norm_track_generic`.

- [norm_track_lobanov](https://jofrhwld.github.io/tidynorm/reference/norm_track_lobanov.md)

- [norm_track_nearey](https://jofrhwld.github.io/tidynorm/reference/norm_track_nearey.md)

- [norm_track_deltaF](https://jofrhwld.github.io/tidynorm/reference/norm_track_deltaF.md)

- [norm_track_wattfab](https://jofrhwld.github.io/tidynorm/reference/norm_track_wattfab.md)

- [norm_track_barkz](https://jofrhwld.github.io/tidynorm/reference/norm_track_barkz.md)

This will normalize vowel formant tracks in the following steps:

1.  Any `.pre_trans` transformations will be applied to the formant
    data.

2.  The Discrete Cosine Transform will be applied to the formant data.

3.  Location `.L` and Scale `.S` expressions will be used to summarize
    the zero^(th) DCT coefficients.

4.  These location and scale will be used to normalize the DCT
    coefficients.

5.  If `.return_dct = TRUE`, these normalized DCT coefficients will be
    returned. Otherwise, the Inverse Discrete Cosine Transform will be
    applied to the normalized DCT coefficients.

6.  Any `.post_trans` transformations will be applied.

### Location and Scale expressions

All normalization procedures built on norm_track_generic work by
normalizing DCT coefficients directly. If \\F_k\\ is the k^(th) DCT
coefficient the normalization procedure is

\$\$ \hat{F}\_k = \frac{F_k - L'}{\sqrt{2}S} \$\$ \$\$ L' =
\begin{cases} L & \text{for }k=0\\ 0 & \text{for }k\>0 \end{cases} \$\$

Rather than requiring users to remember to multiply expressions for
\\S\\ by \\\sqrt{2}\\, this is done by norm_track_generic itself, to
allow greater parallelism with how
[norm_generic](https://jofrhwld.github.io/tidynorm/reference/norm_generic.md)
works.

**Note**: If you want to scale values by a constant in the
normalization, you'll need to divide the constant by `sqrt(2)`.
Post-normalization scaling (e.g. re-scaling to formant-like values) is
probably best handled with a function passed to `.post_trans`.

The expressions for calculating \\L\\ and \\S\\ can be passed to `.L`
and `.S`, respectively. Available values for these expressions are

- `.formant`:

  The original formant value

- `.formant_num`:

  The number of the formant. (e.g. 1 for F1, 2 for F2 etc)

Along with any data columns from your original data.

### Identifying tokens

Track normalization requires identifying individual tokens, so there
must be a column that uniquely identifies (or, in combination with a
`.by` grouping, uniquely identifies) each individual token. This column
should be passed to `.token_id_col`.

### Order

The number of DCT coefficients used is defined by `.order`. The default
value is 5. Larger numbers will lead to less smoothing, and smaller
numbers will lead to more smoothing.

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
  norm_track_generic(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = TRUE),
    .S = mad(.formant, na.rm = TRUE),
    .time_col = t,
    .drop_orig = TRUE,
    .names = "{.formant}_mad"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_generic()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_mad`, `F2_mad`, and `F3_mad`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - median(.formant, na.rm = TRUE))/mad(.formant, na.rm = TRUE)
#> 

if (ggplot2_inst) {
  track_norm |>
    ggplot(
      aes(F2_mad, F1_mad, color = speaker)
    ) +
    stat_density_2d(bins = 4) +
    scale_x_reverse() +
    scale_y_reverse() +
    scale_color_brewer(palette = "Dark2") +
    coord_fixed()
}


# returning the DCT coefficients
track_norm_dct <- track_subset |>
  norm_track_generic(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = TRUE),
    .S = mad(.formant, na.rm = TRUE),
    .time_col = t,
    .drop_orig = TRUE,
    .return_dct = TRUE,
    .names = "{.formant}_mad"
  )

track_norm_means <- track_norm_dct |>
  summarise(
    .by = c(speaker, vowel, .param),
    across(
      ends_with("_mad"),
      mean
    )
  ) |>
  reframe_with_idct(
    ends_with("_mad"),
    .by = speaker,
    .token_id_col = vowel,
    .param_col = .param
  )


if (ggplot2_inst) {
  track_norm_means |>
    ggplot(
      aes(F2_mad, F1_mad, color = speaker)
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
