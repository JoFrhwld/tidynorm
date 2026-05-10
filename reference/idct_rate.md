# Inverse Discrete Cosine Transform Rate

The first derivative of the Inverse DCT

## Usage

``` r
idct_rate(y, n = length(y))
```

## Arguments

- y:

  DCT coefficients

- n:

  The desired length of the idct

## Value

A vector with the first derivative of the inverse DCT

## Details

Returns the first derivative (rate of change) of the Inverse DCT (see
[dct](https://jofrhwld.github.io/tidynorm/reference/dct.md) for more
details).

\$\$ \frac{\delta x_j}{\delta j} = -2\frac{\pi k}{J}\sum\_{k=1}^{N-1}
y_k \sin\left(\frac{\pi k(2j+1)}{2J}\right) \$\$

## Examples

``` r
x <- seq(0, 1, length = 10)
y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

dct_coefs <- dct(y)
y_rate <- idct_rate(dct_coefs)

plot(y)

plot(y_rate)

```
