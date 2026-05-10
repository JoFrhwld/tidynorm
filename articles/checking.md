# Checking Normalization Procedures

``` r

library(tidynorm)
```

If you’ve carried out a few different normalization procedures on a
single dataset, you can double check what the sequence of operations was
with
[`check_norm()`](https://jofrhwld.github.io/tidynorm/reference/check_norm.md).
For example, in this workflow, we’ve given the new normalized data
columns uninformative names.

``` r

norm_data <- speaker_data |>
  norm_lobanov(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_norm1",
    .silent = T
  ) |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .names = "{.formant}_norm2",
    .silent = T
  )
```

We can review which normalization procedure produced which normalized
column like so.

``` r

check_norm(norm_data)
#> Normalization Step
#> • normalized with `tidynorm::norm_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_norm1`, `F2_norm1`, and `F3_norm1`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/(sd(.formant, na.rm = T))
#> 
#> 
#> Normalization Step
#> • normalized with `tidynorm::norm_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_norm2`, `F2_norm2`, and `F3_norm2`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • Transformation prior to normalization: log
#> • (.formant - mean(.formant, na.rm = T))/(1)
```
