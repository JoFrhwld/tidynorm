# Delta F Normalize

Delta F Normalize

## Usage

``` r
norm_deltaF(
  .data,
  ...,
  .by = NULL,
  .by_formant = FALSE,
  .drop_orig = FALSE,
  .keep_params = FALSE,
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

A data frame of Delta F normalized formant values.

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
ggplot2_inst <- require(ggplot2)

speaker_data_deltaF <- speaker_data |>
  norm_deltaF(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_df"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_deltaF()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_df`, `F2_df`, and `F3_df`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - 0)/(mean(.formant/(.formant_num - 0.5), na.rm = T))
#> 

if (ggplot2_inst) {
  ggplot(
    speaker_data_deltaF,
    aes(
      F2_df,
      F1_df,
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
