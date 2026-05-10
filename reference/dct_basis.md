# DCT Basis

The Discrete Cosine Transform basis functions

## Usage

``` r
dct_basis(n, k)
```

## Arguments

- n:

  The length of the basis.

- k:

  The number of basis functions.

## Value

A \\n\times k\\ matrix

## Details

This function will generate the DCT basis functions.

## Examples

``` r
basis <- dct_basis(100, 5)
matplot(basis, type = "l", lty = 1)

```
