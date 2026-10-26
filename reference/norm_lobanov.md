# Lobanov Normalize

Lobanov Normalize

## Usage

``` r
norm_lobanov(
  .data,
  ...,
  .by = NULL,
  .by_formant = TRUE,
  .agg_by = NULL,
  .agg_fun = function(x) mean(x, na.rm = T),
  .drop_orig = FALSE,
  .keep_params = FALSE,
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

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to group by. Typically a column of speaker IDs.

- .by_formant:

  Ignored by this procedure

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

A data frame of Lobanov normalized formant values.

## Details

\$\$ \hat{F}\_{ij} = \frac{F\_{ij} - L_i}{S_i} \$\$

\$\$ L_i = \frac{1}{N}\sum\_{j=1}^{N}F\_{ij} \$\$

\$\$ S_i = \sqrt{\frac{\sum(F\_{ij}-L_i)^2}{N-1}} \$\$

Where

- \\\hat{F}\\ is the normalized formant

- \\i\\ is the formant number

- \\j\\ is the token number

### Pre-aggregation

To address imbalanced data across vowel classes (Lobanov 2.0 (Brand et
al., 2021)), you can pre-aggregate formant values by passing vowel class
column(s) to `.agg_by`, and and an aggregating function to `.agg_fun`.
The aggregation will be done after any `.pre_trans` transformation is
applied, and will be done either formant intrinsically or formant
extrinsically based on the setting passed to `.by_formant`.

## References

Lobanov, B. (1971). Classification of Russian vowels spoken by different
listeners. Journal of the Acoustical Society of America, 49, 606–608.

Brand, J., Hay, J., Clark, L., Watson, K., & Sóskuthy, M. (2021).
Systematic co-variation of monophthongs across speakers of New Zealand
English. Journal of Phonetics, 88, 101096.
<https://doi.org/10.1016/j.wocn.2021.101096>

## Examples

``` r
library(tidynorm)
ggplot2_inst <- require(ggplot2)

speaker_data_lobanov <- speaker_data |>
  norm_lobanov(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_z"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/(sd(.formant, na.rm = T))
#> 

if (ggplot2_inst) {
  ggplot(
    speaker_data_lobanov,
    aes(
      F2_z,
      F1_z,
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
