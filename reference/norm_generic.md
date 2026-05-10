# Generic Normalization Procedure

This is a generic normalization procedure with which you can create your
own normalization method.

## Usage

``` r
norm_generic(
  .data,
  ...,
  .by = NULL,
  .by_formant = FALSE,
  .by_token = FALSE,
  .L = 0,
  .S = 1,
  .pre_trans = identity,
  .post_trans = identity,
  .drop_orig = FALSE,
  .keep_params = FALSE,
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

- .by:

  [`<tidy-select>`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)
  A selection of columns to group by. Typically a column of speaker IDs.

- .by_formant:

  Whether or not the normalization method is formant intrinsic.

- .by_token:

  Whether or not the normalization method is vowel intrinsic

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

- .call:

  Used for internal purposes.

## Value

A data frame of normalized formant values

## Details

The following `norm_*` procedures are built on top of `norm_generic()`.

- [norm_lobanov](https://jofrhwld.github.io/tidynorm/reference/norm_lobanov.md)

- [norm_nearey](https://jofrhwld.github.io/tidynorm/reference/norm_nearey.md)

- [norm_deltaF](https://jofrhwld.github.io/tidynorm/reference/norm_deltaF.md)

- [norm_wattfab](https://jofrhwld.github.io/tidynorm/reference/norm_wattfab.md)

- [norm_barkz](https://jofrhwld.github.io/tidynorm/reference/norm_barkz.md)

### Location and Scale expressions

All normalization procedures built on norm_generic produce normalized
formant values (\\\hat{F}\\) by subtracting a location parameter (\\L\\)
and dividing by a scale parameter (\\S\\).

\$\$ \hat{F} = \frac{F-L}{S} \$\$

The expressions for calculating \\L\\ and \\S\\ can be passed to `.L`
and `.S`, respectively. Available values for these expressions are

- `.formant`:

  The original formant value

- `.formant_num`:

  The number of the formant. (e.g. 1 for F1, 2 for F2 etc)

Along with any data columns from your original data.

### Pre and Post normalization transforms

To apply any transformations before or after normalization, you can pass
a function to `.pre_trans` and `.post_trans`.

### Formant In/Extrinsic Normalization

If `.by_formant` is `TRUE`, normalization will be formant intrinsic. If
`.by_formant` is `FALSE`, normalization will be formant extrinsic.

### Token In/Extrinsic Normalization

If `.by_token` is `TRUE`, normalization will be token intrinsic. If
`.by_token` is `FALSE`, normalization will be token extrinsic.

## Examples

``` r
library(tidynorm)
library(dplyr)

speaker_data |>
  norm_generic(
    F1:F3,
    .by = speaker,
    .by_formant = TRUE,
    .L = median(.formant, na.rm = TRUE),
    .S = mad(.formant, na.rm = TRUE),
    .drop_orig = TRUE,
    .names = "{.formant}_mad"
  )
#> Normalization info
#> • normalized with `tidynorm::norm_generic()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_mad`, `F2_mad`, and `F3_mad`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - median(.formant, na.rm = TRUE))/(mad(.formant, na.rm = TRUE))
#> 
#> # A tibble: 10,697 × 8
#>    speaker vowel plt_vclass ipa_vclass word      F1_mad  F2_mad F3_mad
#>    <chr>   <chr> <chr>      <chr>      <chr>      <dbl>   <dbl>  <dbl>
#>  1 s01     EY    eyF        ejF        OKAY      1.13    0.400  -0.521
#>  2 s01     AH    uh         ʌ          UM        0.643  -0.166   0.629
#>  3 s01     AY    ay         aj         I'M       2.09   -0.0211  0.165
#>  4 s01     IH    i          ɪ          LIVED    -0.454  -1.13    1.41 
#>  5 s01     IH    i          ɪ          IN       -0.0221  1.04    1.03 
#>  6 s01     AH    @          ə          COLUMBUS -0.0206 -0.104   0.715
#>  7 s01     AY    ay         aj         MY       -0.280   0.0479  0.968
#>  8 s01     IH    i          ɪ          ENTIRE   -0.649   1.07    1.03 
#>  9 s01     ER    *hr        ə˞         ENTIRE   -0.585  -0.709  -2.35 
#> 10 s01     AY    ay0        aj0        LIFE      0.987  -0.656  -2.34 
#> # ℹ 10,687 more rows
```
