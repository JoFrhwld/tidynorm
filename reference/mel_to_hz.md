# Mel to Hz

Convert Mel to Hz

## Usage

``` r
mel_to_hz(mel, htk = FALSE)
```

## Arguments

- mel:

  Numeric values in Hz

- htk:

  Whether or not to use the HTK formula

## Value

A numeric vector of Hz values

## Details

This is a direct re-implementation of the `hz_to_mel` function from the
[librosa](https://librosa.org/) library.

The default method is to use the method due to Slaney (1998), which is
linear below 1000Hz, and logarithmic above.

If `htk=TRUE`, the method from HTK, due to O'Shaughnessy (1987) is used.

## References

McFee, B., C. Raffel, D. Liang, D. PW Ellis, M. McVicar, E. Battenberg,
and O. Nieto. librosa: Audio and music signal analysis in python. In
Proceedings of the 14th python in science conference, pp. 18-25.

O'Shaughnessy, D (1987). Speech communication: human and machine.
Addison-Wesley. p. 150. ISBN 978-0-201-16520-3.

Slaney, M. (1998) Auditory Toolbox: A MATLAB Toolbox for Auditory
Modeling Work. Technical Report, version 2, Interval Research
Corporation.

## Examples

``` r
mel_to_hz(c(7.5, 15, 25, 31))
#> [1]  500.000 1000.000 1988.773 3004.251
```
