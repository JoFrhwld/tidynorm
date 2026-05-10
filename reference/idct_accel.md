# Inverse Discrete Cosine Transform Acceleration

The second derivative of the Inverse DCT

## Usage

``` r
idct_accel(y, n = length(y))
```

## Arguments

- y:

  DCT coefficients

- n:

  The desired length of the idct

## Value

A vector with the second derivative of the inverse DCT

## Details

Returns the second derivative (acceleration) of the Inverse DCT (see
[dct](https://jofrhwld.github.io/tidynorm/reference/dct.md) for more
details).

\$\$ \frac{\delta^2 x_j}{\delta j^2} = -2\left(\frac{\pi
k}{J}\right)^2\sum\_{k=1}^{N-1} y_k \cos\left(\frac{\pi
k(2j+1)}{2J}\right) \$\$

## Examples

``` r
x <- seq(0, 1, length = 10)
y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

dct_coefs <- dct(y)
y_accel <- idct_accel(dct_coefs)

plot(y)

plot(y_accel)

```
