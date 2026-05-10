# Bark to Hz

Converts bark to Hz

## Usage

``` r
bark_to_hz(bark)
```

## Arguments

- bark:

  Frequency in Bark

## Value

A vector of Hz scaled values

## Details

\$\$ \hat{b} = \begin{cases} \frac{b - 0.3}{0.85} & \text{if} ~ b \< 2\\
\frac{b + 4.422}{1.22} & \text{if} ~ b \> 20.1\\ b & \text{otherwise}
\end{cases} \$\$

\$\$ hz = 1960\frac{\hat{b} + 0.53}{26.28 - \hat{b}} \$\$

## References

Traunmüller, H. (1990). Analytical expressions for the tonotopic sensory
scale. The Journal of the Acoustical Society of America, 88(1), 97–100.
[doi:10.1121/1.399849](https://doi.org/10.1121/1.399849)

## Examples

``` r
bark <- seq(1.5, 13, length = 100)
hz <- bark_to_hz(bark)
plot(bark, hz)
```
