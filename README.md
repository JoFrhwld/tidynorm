

<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidynorm <a href="https://jofrhwld.github.io/tidynorm/"><img src="man/figures/logo.png" align="right" height="139" alt="tidynorm website" /></a>

This package is currently not even at a pre-release stage

<!-- badges: start -->

[![R-CMD-check](https://github.com/JoFrhwld/tidynorm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JoFrhwld/tidynorm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `{tidynorm}` is to provide convenient and tidy functions to
normalize vowel formant data.

## Installation

You can install the development version of tidynorm like so:

``` r
remotes::install_github("jofrhwld/tidynorm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tidynorm)
library(ggplot2)
options(
  ggplot2.discrete.colour = \(...) scale_color_brewer(palette = "Dark2", ...)
)

ggplot(
  speaker_data,
  aes(
    F2, F1,
    color = speaker
  )
)+
  stat_density_2d(
    bins = 4
  )+
  scale_x_reverse()+
  scale_y_reverse()+
  coord_fixed()
```

<img src="man/figures/README-unnorm-1.png" style="width:100.0%" />

``` r
speaker_data |> 
  norm_nearey(
    F1:F3,
    .by = speaker,
    .names = "{.col}_nearey"
  ) ->
  speaker_normalized
#> Normalization info
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_nearey`, `F2_nearey`, and `F3_nearey`
#> • grouped by `speaker`
#> • formant extrinsic

speaker_normalized |> 
  ggplot(
    aes(
      F2_nearey, F1_nearey,
      color = speaker
    )
  )+
  stat_density_2d(
    bins = 4
  )+
  scale_x_reverse()+
  scale_y_reverse()+
  coord_fixed()  
```

<img src="man/figures/README-norm-1.png" style="width:100.0%" />
