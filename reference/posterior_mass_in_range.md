# Calculates the amount of probability (mass) a parameter has in a range

Calculates the amount of probability (mass) a parameter has in a range

## Usage

``` r
posterior_mass_in_range(stanfit, param, range)
```

## Arguments

- stanfit:

  A stanfit object e.g. as returned by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).
  The function's name reflects the assumption that this contains samples
  from the posterior, but it could contain samples from the prior.

- param:

  The name of the parameter, as a character.

- range:

  The lower and upper boundaries of the range, as a length-2 numeric
  vector. The boundaries are not considered part of the range (this
  should be irrelevant for continuous parameters). The lower boundary
  can be -Inf for a range that is unbounded from below; the upper
  boundary may be Inf for a range that is unbounded from above.

## Value

A single number: the amount of probability (mass) that this parameter
has in this range, estimated as the fraction of samples in the stanfit
object for which the parameter is in the range.

## Examples

``` r
  eg <- mastiff::stan_example_regression$posterior_samples
  posterior_mass_in_range(eg, "m", c(-Inf, 2))
#> [1] 0
```
