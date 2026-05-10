# Inverse Discrete Cosine Transform

The Inverse DCT

## Usage

``` r
idct(y, n)

# S3 method for class 'numeric'
idct(y, n = length(y))

# S3 method for class 'matrix'
idct(y, n = nrow(y))
```

## Arguments

- y:

  A vector or matrix of DCT coefficients

- n:

  The desired length of the idct

## Value

The returned value depends on the values in `y`.

When passed a numeric vector, returns numeric vector of length `n`.

When passed a matrix, returns a matrix with `n` rows and the same number
of columns as `y`.

## Details

Applies the Inverse DCT (see
[dct](https://jofrhwld.github.io/tidynorm/reference/dct.md) for more
details).

\$\$ x_j = \sqrt{2}y_0 + 2\sum\_{k=1}^{N-1} y_k \cos\left(\frac{\pi
k(2j+1)}{2J}\right) \$\$

## Examples

``` r
x <- seq(0, 1, length = 10)
y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

dct_coefs <- dct(y)
recovered_y <- idct(dct_coefs)

plot(y, recovered_y)

```
