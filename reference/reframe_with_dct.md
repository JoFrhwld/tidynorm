# Reframe with DCT

Reframe data columns using the Discrete Cosine Transform

## Usage

``` r
reframe_with_dct(
  .data,
  ...,
  .token_id_col = NULL,
  .by = NULL,
  .time_col = NULL,
  .order = 5
)
```

## Arguments

- .data:

  A data frame

- ...:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  One or more unquoted expressions separated by commas. These should
  target the vowel formant.

- .token_id_col:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  The token ID column.

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A grouping column.

- .time_col:

  A time column.

- .order:

  The number of DCT parameters to return. If `NA`, all DCT parameters
  will be returned.

## Value

A data frame with with the targeted DCT coefficients, along with two
additional columns

- .param:

  The nth DCT coefficient number

- .n:

  The number of original data values

## Details

This function will tidily apply the Discrete Cosine Transform with
forward normalization (see
[dct](https://jofrhwld.github.io/tidynorm/reference/dct.md) for more
info) to the targeted columns.

### Identifying tokens

The DCT only works on a by-token basis, so there must be a column that
uniquely identifies (or, in combination with a `.by` grouping, uniquely
identifies) each individual token. This column should be passed to
`.token_id_col`.

### Order

The number of DCT coefficients to return is defined by `.order`. The
default value is 5. Larger numbers will lead to less smoothing when the
Inverse DCT is applied (see
[idct](https://jofrhwld.github.io/tidynorm/reference/idct.md)). Smaller
numbers will lead to more smoothing.

If `NA` is passed to `.order`, all DCT parameters will be returned,
which when the Inverse DCT is supplied, will completely reconstruct the
original data.

### Sorting by Time

An optional `.time_col` can also be defined to ensure that the data is
correctly arranged by time.

## Examples

``` r
library(tidynorm)
library(dplyr)

speaker_small <- filter(
  speaker_tracks,
  id == 0
)

speaker_dct <- reframe_with_dct(
  speaker_small,
  F1:F3,
  .by = speaker,
  .token_id_col = id,
  .time_col = t
)

head(
  speaker_dct
)
#> # A tibble: 6 × 10
#>   speaker    id vowel plt_vclass word  .param     F1       F2     F3    .n
#>   <chr>   <dbl> <chr> <chr>      <chr>  <dbl>  <dbl>    <dbl>  <dbl> <int>
#> 1 s01         0 EY    eyF        OKAY       0 495.   1624.    2108.     20
#> 2 s01         0 EY    eyF        OKAY       1 -21.9  -118.     -46.1    20
#> 3 s01         0 EY    eyF        OKAY       2  57.3    24.1     26.8    20
#> 4 s01         0 EY    eyF        OKAY       3   8.11   16.8    -25.8    20
#> 5 s01         0 EY    eyF        OKAY       4  -5.46   -0.529   19.0    20
#> 6 s03         0 AY    ay         I          0 508.    909.    1556.     20
```
