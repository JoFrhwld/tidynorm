# Normalization Methods

``` r

library(tidynorm)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
```

In addition to the generic normalization functions in tidynorm
([`norm_generic()`](https://jofrhwld.github.io/tidynorm/reference/norm_generic.md),
[`norm_track_generic()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_generic.md)
and
[`norm_dct_generic()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_generic.md)),
there are a number of convenience functions for a few established
normalization methods.

## Lobanov ([Lobanov 1971](#ref-lobanov))

tidynorm functions:

- [`norm_lobanov()`](https://jofrhwld.github.io/tidynorm/reference/norm_lobanov.md)
- [`norm_track_lobanov()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_lobanov.md)
- [`norm_dct_lobanov()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_lobanov.md)

Lobanov normalization z-scores each formant. If F\_{ij} is the j^{th}
token of the i^{th} formant, and \hat{F}\_{ij} is its normalized value,
then

\hat{F}\_{ij} = \frac{F\_{ij} - L_i}{S_i}

Where L_i is the mean across the i^{th} formant:

L_i = \frac{1}{N}\sum\_{j=1}^N F\_{ij}

And S_i is the standard deviation across the i^{th} formant.

S_i = \sqrt{\frac{\sum_j(F\_{ij}-L_i)^2}{N-1}}

### Using the Lobanov normalization functions

#### On points

``` r

point_norm <- speaker_data |>
  norm_lobanov(
    F1:F3,
    .by = speaker
  )
#> Normalization info
#> • normalized with `tidynorm::norm_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/(sd(.formant, na.rm = T))
```

#### On tracks

``` r

track_norm <- speaker_tracks |>
  norm_track_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/sd(.formant, na.rm = T)
```

#### On DCT Coefficients

``` r

dct_norm <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_lobanov(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )
#> Normalization info
#> • normalized with `tidynorm::norm_dct_lobanov()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_z`, `F2_z`, and `F3_z`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - mean(.formant, na.rm = T))/sd(.formant, na.rm = T)
```

## Nearey Normalization ([Nearey 1978](#ref-neareyPhoneticFeatureSystems1978))

tidynorm functions:

- [`norm_nearey()`](https://jofrhwld.github.io/tidynorm/reference/norm_nearey.md)
- [`norm_track_nearey()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_nearey.md)
- [`norm_dct_nearey()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_nearey.md)

Nearey Normalization first log transforms formant values, then subtracts
the grand mean across all formants. If F\_{ij} is the j^{th} token of
the i^{th} formant, and \hat{F}\_{ij} is its normalized value, then

\hat{F}\_{ij} = \log(F\_{ij}) - L

L = \frac{1}{MN}\sum\_{i = 1}^M\sum\_{j=1}^N \log(F\_{ij})

The fact that the grand mean is taken across all formants, it’s
important to report whether just F1 and F2 were used, or if F1, F2 and
F3 were used.

### Using the Nearey normalization functions

#### On points

``` r

point_norm <- speaker_data |>
  norm_nearey(
    F1:F3,
    .by = speaker
  )
#> Normalization info
#> • normalized with `tidynorm::norm_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_lm`, `F2_lm`, and `F3_lm`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • Transformation prior to normalization: log
#> • (.formant - mean(.formant, na.rm = T))/(1)
```

#### On tracks

``` r

track_norm <- speaker_tracks |>
  norm_track_nearey(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_nearey()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_lm`, `F2_lm`, and `F3_lm`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • Transformation prior to normalization: log
#> • (.formant - mean(.formant, na.rm = T))/(1/sqrt(2))
```

#### On DCT Coefficients

``` r

dct_norm <- speaker_tracks |>
  mutate(across(F1:F3, log)) |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
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
```

## Delta F ([Johnson 2020](#ref-johnsonDFMethodVocal2020))

tidynorm functions:

- [`norm_deltaF()`](https://jofrhwld.github.io/tidynorm/reference/norm_deltaF.md)

- [`norm_track_deltaF()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_deltaF.md)

- [`norm_dct_deltaF()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_deltaF.md)

The \Delta F normalization method is based on the average of formant
spacing. If F\_{ij} is the j^{th} token of the i^{th} formant, and
\hat{F}\_{ij} is its normalized value, then

\hat{F} = \frac{F\_{ij}}{S}

S = \frac{1}{MN} \sum\_{i=1}^M\sum\_{j=1}^N \frac{F\_{ij}}{i-0.5}

The fact that this method takes a weighted average across all formants,
it’s important to report whether just F1 and F2 were used, or if F1, F2
and F3 were used.

### Using the DeltaF normalization functions

#### On points

``` r

point_norm <- speaker_data |>
  norm_deltaF(
    F1:F3,
    .by = speaker
  )
#> Normalization info
#> • normalized with `tidynorm::norm_deltaF()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_df`, `F2_df`, and `F3_df`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - 0)/(mean(.formant/(.formant_num - 0.5), na.rm = T))
```

#### On tracks

``` r

track_norm <- speaker_tracks |>
  norm_track_deltaF(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_deltaF()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_df`, `F2_df`, and `F3_df`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - 0)/mean(.formant/(.formant_num - 0.5), na.rm = T)
```

#### On DCT coefficients

``` r

dct_norm <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_deltaF(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )
#> Normalization info
#> • normalized with `tidynorm::norm_dct_deltaF()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_df`, `F2_df`, and `F3_df`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • (.formant - 0)/mean(.formant/(.formant_num - 0.5), na.rm = T)
```

## Watt & Fabricious ([Watt and Fabricius 2002](#ref-wattEvaluationTechniqueImproving2002))

tidynorm functions:

- [`norm_wattfab()`](https://jofrhwld.github.io/tidynorm/reference/norm_wattfab.md)

- [`norm_track_wattfab()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_wattfab.md)

- [`norm_dct_wattfab()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_wattfab.md)

The Watt & Fabricious method attempt to center vowel spaces on their
“center of gravity”. The original Watt & Fabricious method involved
calculating average F1 and F2 values for point vowels. In tidynorm, a
modified version has been implemented that just uses the average over F1
and F2 as the centers of gravity. If F\_{ij} is the j^{th} token of the
i^{th} formant, and \hat{F}\_{ij} is its normalized value, then

\hat{F\_{ij}} = \frac{F\_{ij}}{S_i}

Where S_i is the mean across the i\_{th} formant.

S_i = \frac{1}{N} \sum\_{j = 1}^N F\_{ij}

### Using the Watt & Fabricious normaliation functions

#### On points

``` r

point_norm <- speaker_data |>
  norm_wattfab(
    F1:F3,
    .by = speaker
  )
#> Normalization info
#> • normalized with `tidynorm::norm_wattfab()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_wf`, `F2_wf`, and `F3_wf`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - 0)/(mean(.formant, na.rm = T))
```

#### On tracks

``` r

track_norm <- speaker_tracks |>
  norm_track_wattfab(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_wattfab()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_wf`, `F2_wf`, and `F3_wf`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - 0)/mean(.formant, na.rm = T)
```

#### On DCT coefficients

``` r

dct_norm <- speaker_tracks |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_wattfab(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )
#> Normalization info
#> • normalized with `tidynorm::norm_dct_wattfab()`
#> • normalized `F1`, `F2`, and `F3`
#> • normalized values in `F1_wf`, `F2_wf`, and `F3_wf`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: TRUE
#> • (.formant - 0)/mean(.formant, na.rm = T)
```

## Bark Difference ([Syrdal and Gopal 1986](#ref-syrdalPerceptualModelVowel1986))

tidynorm functions

- [`norm_barkz()`](https://jofrhwld.github.io/tidynorm/reference/norm_barkz.md)

- [`norm_track_barkz()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_barkz.md)

- [`norm_dct_barkz()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_barkz.md)

The bark difference metric tries to normalize vowels on the basis of
individual tokens. First, formant data is converted to bark (see
[`hz_to_bark()`](https://jofrhwld.github.io/tidynorm/reference/hz_to_bark.md)),
then F3 is subtracted from F1 and F2. If F\_{ij} is the j^{th} token of
the i^{th} formant, and \hat{F}\_{ij} is its normalized value, then

\hat{F}\_{ij} = \text{bark}(F\_{ij}) - L_j

L_j = \text{bark}(F\_{3j})

### Using the Bark Difference normalization functions

#### On points

``` r

point_norm <- speaker_data |>
  norm_barkz(
    F1:F3,
    .by = speaker
  )
#> Normalization info
#> • normalized with `tidynorm::norm_barkz()`
#> • normalized `F1`, `F2`, and `F3`
#> • `F3` used for third formant.
#> • normalized values in `F1_bz`, `F2_bz`, and `F3_bz`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • within token: TRUE
#> • Transformation prior to normalization: hz_to_bark
#> • (.formant - .formant[3])/(1)
```

#### On tracks

``` r

track_norm <- speaker_tracks |>
  norm_track_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  )
#> Normalization info
#> • normalized with `tidynorm::norm_track_barkz()`
#> • normalized `F1`, `F2`, and `F3`
#> • `F3` used for third formant.
#> • normalized values in `F1_bz`, `F2_bz`, and `F3_bz`
#> • token id column: `id`
#> • time column: `t`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • within token: TRUE
#> • Transformation prior to normalization: hz_to_bark
#> • (.formant - .formant[3])/(1/sqrt(2))
```

#### On DCT Coefficients

``` r

dct_norm <- speaker_tracks |>
  mutate(
    across(F1:F3, hz_to_bark)
  ) |>
  reframe_with_dct(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .time_col = t
  ) |>
  norm_dct_barkz(
    F1:F3,
    .by = speaker,
    .token_id_col = id,
    .param_col = .param
  )
#> Normalization info
#> • normalized with `tidynorm::norm_dct_barkz()`
#> • normalized `F1`, `F2`, and `F3`
#> • `F3` used for third formant.
#> • normalized values in `F1_bz`, `F2_bz`, and `F3_bz`
#> • token id column: `id`
#> • DCT parameter column: `.param`
#> • grouped by `speaker`
#> • within formant: FALSE
#> • within token: TRUE
#> • (.formant - .formant[3])/(1/sqrt(2))
```

## References

Johnson, Keith. 2020. “The ΔF Method of Vocal Tract Length Normalization
for Vowels.” *Laboratory Phonology: Journal of the Association for
Laboratory Phonology* 11 (11): 10.
<https://doi.org/10.5334/labphon.196>.

Lobanov, Boris. 1971. “Classification of Russian Vowels Spoken by
Different Listeners.” *Journal of the Acoustical Society of America* 49:
606–8. <https://doi.org/10.1121/1.1912396>.

Nearey, Terrance M. 1978. “Phonetic Feature Systems for Vowels.” PhD
thesis, University of Alberta.

Syrdal, Ann K., and H. S. Gopal. 1986. “A Perceptual Model of Vowel
Recognition Based on the Auditory Representation of American English
Vowels.” *The Journal of the Acoustical Society of America* 79 (4):
1086–100. <https://doi.org/10.1121/1.393381>.

Watt, Dominic, and Anne Fabricius. 2002. “Evaluation of a Technique for
Improving the Mapping of Multiple Speakers’ Vowel Spaces in the F1   F2
Plane.” *Leeds Working Papers in Linguistics and Phonetics* 9: 159–73.
