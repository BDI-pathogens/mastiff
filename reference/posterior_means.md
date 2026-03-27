# Calculates the mean or median for each parameter for each stanfit object

In presto the typical list supplied to this would have one stanfit
object for each simulated trial.

## Usage

``` r
posterior_means(stanfit_list, params_desired = NA)

posterior_medians(stanfit_list, params_desired = NA)
```

## Arguments

- stanfit_list:

  A list of stanfit objects e.g. as returned by
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).
  The function's name reflects the assumption that these contain samples
  from the posterior, but they could contain samples from the prior.

- params_desired:

  A character vector with the names of those parameters in the stanfit
  object that should be included in the output. The default value of
  `NA` means all parameters are included except `"lp__"` (which is not
  actually a parameter: Stan uses it to record the value of the target
  probability density).

## Value

A 2D array: the first index for the parameter, the second index for the
stanfit object.

## Examples

``` r
   eg <- mastiff::stan_example_regression
   posterior_means(list(eg$posterior_samples, eg$prior_samples))
#>           [,1]      [,2]
#> m     3.008395 3.0022141
#> c     2.080654 1.9822194
#> sigma 1.007373 0.9948005
   posterior_medians(list(eg$posterior_samples, eg$prior_samples))
#>           [,1]      [,2]
#> m     3.008574 3.0191510
#> c     2.081751 1.9318612
#> sigma 0.983071 0.9844412
```
