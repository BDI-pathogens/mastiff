# Calculates central probability intervals for each parameter for each stanfit object

The function is a trivial wrapper around
[`rstantools::posterior_interval()`](https://mc-stan.org/rstantools/reference/posterior_interval.html)
for a list of stanfit objects.

## Usage

``` r
posterior_intervals(stanfit_list, prob, params_desired = NA, ...)
```

## Arguments

- stanfit_list:

  A list of stanfit objects e.g. as returned by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).
  The function's name reflects the assumption that these contain samples
  from the posterior, but they could contain samples from the prior.

- prob:

  A single number between 0 and 1 specifying the amount of probability
  to be contained in the interval. e.g. Specifying 0.95 results in an
  interval that spans the 2.5th to 97.5th percentiles of the
  distribution.

- params_desired:

  A character vector with the names of those parameters in the stanfit
  object that should be included in the output. The default value of
  `NA` means all parameters are included except `"lp__"` (which is not
  actually a parameter: Stan uses it to record the value of the target
  probability density).

- ...:

  Additional arguments to pass to
  [`rstantools::posterior_interval()`](https://mc-stan.org/rstantools/reference/posterior_interval.html).

## Value

A 3D array: the first index for the parameter, the second for the
percentile, the third for the stanfit object.

## Examples

``` r
   eg <- mastiff::stan_example_regression
   posterior_intervals(list(eg$posterior_samples, eg$prior_samples), prob =
   0.95)
#> , , 1
#> 
#>            2.5%    97.5%
#> m     2.9321956 3.084793
#> c     1.1434339 2.989257
#> sigma 0.7249795 1.425864
#> 
#> , , 2
#> 
#>             2.5%    97.5%
#> m     0.18343293 5.820427
#> c     0.09083891 3.891102
#> sigma 0.05382647 1.959117
#> 
```
