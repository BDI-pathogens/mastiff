# logistic(x) = 1 / (1 + exp(-x)), a wrapper for [`stats::plogis()`](https://rdrr.io/r/stats/Logistic.html)

logistic(x) = 1 / (1 + exp(-x)), a wrapper for
[`stats::plogis()`](https://rdrr.io/r/stats/Logistic.html)

## Usage

``` r
logistic(...)
```

## Arguments

- ...:

  A numeric vector of values, and other named arguments to
  [`stats::plogis()`](https://rdrr.io/r/stats/Logistic.html) if desired.

## Value

A numeric vector

## Examples

``` r
logistic(2)
#> [1] 0.8807971
```
