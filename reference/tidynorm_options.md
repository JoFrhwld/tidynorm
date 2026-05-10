# Set tidynorm options

Set tidynorm verbosity

## Usage

``` r
tidynorm_options(
  .silent = opt("tidynorm.silent"),
  .warnings = opt("tidynorm.warnings")
)
```

## Arguments

- .silent:

  Suppress normalization information messages when running a `norm_*()`
  function. (Defaults to `FALSE`, overwritable using option
  'tidynorm.silent' or environment variable
  'R_TIDYNORM_TIDYNORM_SILENT')

- .warnings:

  Print warnings from tidynorm functions. (Defaults to `TRUE`,
  overwritable using option 'tidynorm.warnings' or environment variable
  'R_TIDYNORM_TIDYNORM_WARNINGS')

## See also

[options](https://jofrhwld.github.io/tidynorm/reference/options.md)

## Examples

``` r
tidynorm_options(.silent = TRUE, .warnings = FALSE)

speaker_data |>
  norm_generic(F1:F3) ->
  norm1

tidynorm_options(.silent = FALSE, .warnings = TRUE)

speaker_data |>
  norm_generic(F1:F3) ->
  norm2
#> Warning: There is was no grouping provided.
#> ℹ You may want to provide `.by` with a speaker id column.
#> 
#> Normalization info
#> • normalized with `tidynorm::norm_generic()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_n`, `F2_n`, and `F3_n`
#> • grouped by
#> • within formant: FALSE
#> • (.formant - 0)/(1)
#> 
```
