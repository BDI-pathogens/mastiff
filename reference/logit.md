# logit(p) = log(p / (1 - p)), a wrapper for [`stats::qlogis()`](https://rdrr.io/r/stats/Logistic.html)

logit(p) = log(p / (1 - p)), a wrapper for
[`stats::qlogis()`](https://rdrr.io/r/stats/Logistic.html)

## Usage

``` r
logit(...)
```

## Arguments

- ...:

  A numeric vector of values between 0 and 1, and other named arguments
  to [`stats::qlogis()`](https://rdrr.io/r/stats/Logistic.html) if
  desired.

## Value

A numeric vector

## Examples

``` r
logit(0.5)
#> [1] 0
logit(log(0.5), log.p = TRUE)
#> [1] 0
```
