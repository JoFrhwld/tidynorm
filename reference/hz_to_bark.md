# Hz to Bark

Converts Hz to Bark

## Usage

``` r
hz_to_bark(hz)
```

## Arguments

- hz:

  Frequency in Hz

## Value

A vector of bark scaled values

## Details

\$\$ \hat{b} = \frac{26.81 hz}{1960 + hz} - 0.53 \$\$ \$\$ b =
\begin{cases} \hat{b} + 0.15(2-\hat{b}) & \text{if}~\hat{b} \< 2\\
\hat{b} + 0.22(\hat{b} - 20.1) & \text{if}~\hat{b} \> 20.1\\ \hat{b} &
\text{otherwise} \end{cases} \$\$

## References

Traunmüller, H. (1990). Analytical expressions for the tonotopic sensory
scale. The Journal of the Acoustical Society of America, 88(1), 97–100.
[doi:10.1121/1.399849](https://doi.org/10.1121/1.399849)

## Examples

``` r
hz <- seq(150, 2000, length = 100)
bark <- hz_to_bark(hz)
plot(hz, bark)

```
