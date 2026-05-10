# Discrete Cosine Transform

Discrete Cosine Transform

## Usage

``` r
dct(x)

# S3 method for class 'numeric'
dct(x)

# S3 method for class 'matrix'
dct(x)
```

## Arguments

- x:

  A vector or matrix to which the discrete cosine transform is applied

## Value

Returned value depends on `x`.

When passed a numeric vector, returns a numeric vector the same size as
`x` with the DCT Coefficients.

When passed a matrix, returns a matrix the same size as `x` with the DCT
Coefficients.

## Details

The DCT definitions here are based on the python `scipy.fft.dct`
definitions. Specifically this use:

    # python code
    scipy.fft.dct(x, norm = "forward", orthogonalize = True)

\$\$ y_k = \frac{1}{zN} \sum\_{j=0}^{N-1}x_j\cos\left(\frac{\pi
k(2j+1)}{2N}\right) \$\$

\$\$ z = \begin{cases} \sqrt{2}& \text{for }k=0\\ 1 & \text{for }k\>0
\end{cases} \$\$

For the Inverse Discrete Cosine Transform, see
[idct](https://jofrhwld.github.io/tidynorm/reference/idct.md).

## Examples

``` r
x <- seq(0, 1, length = 10)
y <- 5 + x + (2 * (x^2)) + (-2 * (x^4))

dct_coefs <- dct(y)
```
