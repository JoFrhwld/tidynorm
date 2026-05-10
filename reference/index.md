# Package index

## Package Overview

- [`tidynorm`](https://jofrhwld.github.io/tidynorm/reference/tidynorm-package.md)
  [`tidynorm-package`](https://jofrhwld.github.io/tidynorm/reference/tidynorm-package.md)
  : tidynorm: Tools for Tidy Vowel Normalization
- [`options`](https://jofrhwld.github.io/tidynorm/reference/options.md)
  : tidynorm Options

## Normalization Procedures

Tidy normalization procedures to be applied to your formant data frames.

### Point measurements

- [`norm_generic()`](https://jofrhwld.github.io/tidynorm/reference/norm_generic.md)
  : Generic Normalization Procedure
- [`norm_barkz()`](https://jofrhwld.github.io/tidynorm/reference/norm_barkz.md)
  : Bark Difference Normalize
- [`norm_deltaF()`](https://jofrhwld.github.io/tidynorm/reference/norm_deltaF.md)
  : Delta F Normalize
- [`norm_lobanov()`](https://jofrhwld.github.io/tidynorm/reference/norm_lobanov.md)
  : Lobanov Normalize
- [`norm_nearey()`](https://jofrhwld.github.io/tidynorm/reference/norm_nearey.md)
  : Nearey Normalize
- [`norm_wattfab()`](https://jofrhwld.github.io/tidynorm/reference/norm_wattfab.md)
  : Watt & Fabricius Normalize

### Formant tracks

- [`norm_track_generic()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_generic.md)
  : Generic Formant Track Normalization Procedure
- [`norm_track_barkz()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_barkz.md)
  : Bark Difference Track Normalization
- [`norm_track_deltaF()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_deltaF.md)
  : Delta F Track Normalization
- [`norm_track_lobanov()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_lobanov.md)
  : Lobanov Track Normalization
- [`norm_track_nearey()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_nearey.md)
  : Nearey Track Normalization
- [`norm_track_wattfab()`](https://jofrhwld.github.io/tidynorm/reference/norm_track_wattfab.md)
  : Watt and Fabricius Track normalization

### DCT Coefficients

- [`norm_dct_generic()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_generic.md)
  : Generic Formant DCT Normalization Procedure
- [`norm_dct_barkz()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_barkz.md)
  : Bark Difference DCT Normalization
- [`norm_dct_deltaF()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_deltaF.md)
  : Delta F DCT Normalization
- [`norm_dct_lobanov()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_lobanov.md)
  : Lobanov DCT Normalization
- [`norm_dct_nearey()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_nearey.md)
  : Nearey DCT Normalization
- [`norm_dct_wattfab()`](https://jofrhwld.github.io/tidynorm/reference/norm_dct_wattfab.md)
  : Watt and Fabricius DCT normalization

## DCT

### Basic DCT Functions

- [`dct()`](https://jofrhwld.github.io/tidynorm/reference/dct.md) :
  Discrete Cosine Transform
- [`idct()`](https://jofrhwld.github.io/tidynorm/reference/idct.md) :
  Inverse Discrete Cosine Transform
- [`idct_rate()`](https://jofrhwld.github.io/tidynorm/reference/idct_rate.md)
  : Inverse Discrete Cosine Transform Rate
- [`idct_accel()`](https://jofrhwld.github.io/tidynorm/reference/idct_accel.md)
  : Inverse Discrete Cosine Transform Acceleration
- [`dct_basis()`](https://jofrhwld.github.io/tidynorm/reference/dct_basis.md)
  : DCT Basis

### Tidy DCT Functions

- [`reframe_with_dct()`](https://jofrhwld.github.io/tidynorm/reference/reframe_with_dct.md)
  : Reframe with DCT
- [`reframe_with_idct()`](https://jofrhwld.github.io/tidynorm/reference/reframe_with_idct.md)
  : Reframe with IDCT
- [`reframe_with_dct_smooth()`](https://jofrhwld.github.io/tidynorm/reference/reframe_with_dct_smooth.md)
  : Reframe with DCT Smooth

## Transforms

- [`hz_to_bark()`](https://jofrhwld.github.io/tidynorm/reference/hz_to_bark.md)
  : Hz to Bark
- [`hz_to_mel()`](https://jofrhwld.github.io/tidynorm/reference/hz_to_mel.md)
  : Hz to Mel
- [`bark_to_hz()`](https://jofrhwld.github.io/tidynorm/reference/bark_to_hz.md)
  : Bark to Hz
- [`mel_to_hz()`](https://jofrhwld.github.io/tidynorm/reference/mel_to_hz.md)
  : Mel to Hz

## Utility Functions

- [`check_norm()`](https://jofrhwld.github.io/tidynorm/reference/check_norm.md)
  : Check Normalization Procedures
- [`tidynorm_options()`](https://jofrhwld.github.io/tidynorm/reference/tidynorm_options.md)
  : Set tidynorm options

## Data

- [`speaker_data`](https://jofrhwld.github.io/tidynorm/reference/speaker_data.md)
  : Speaker Data
- [`speaker_tracks`](https://jofrhwld.github.io/tidynorm/reference/speaker_tracks.md)
  : Speaker Tracks
