# Check Normalization Procedures

`check_norm()` will generate a message with information about which
normalization procedures have been applied to the data.

## Usage

``` r
check_norm(.data)
```

## Arguments

- .data:

  A data frame produced by a tidynorm function.

## Value

This only prints an info message.

## Examples

``` r
speaker_norm <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker,
    .silent = TRUE
  )

check_norm(speaker_norm)
#> Normalization Step
#> • normalized with `tidynorm::norm_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_lm`, `F2_lm`, and `F3_lm`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • Transformation prior to normalization: log
#> • (.formant - mean(.formant, na.rm = T))/(1)
#> 
```
