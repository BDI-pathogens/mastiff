# Estimate the parameters of a mixture of two normal distributions

Estimate the parameters of a mixture of two normal distributions

## Usage

``` r
estimate_mixture_of_two_normals(
  y,
  prior_boundaries,
  groups = NA,
  sample_posterior_not_prior = TRUE,
  cores = 1,
  report_stan_progress = FALSE,
  ...
)
```

## Arguments

- y:

  A numeric vector of observations (values drawn from the distribution).

- prior_boundaries:

  A data.frame specifying the upper and lower boundaries for the priors
  of the params. It should include columns "param", "lower", and
  "upper". The "param" column should include values "mu_0", "mu_1",
  "sd_0", "sd_1", "sd_groups" and "p".

- groups:

  A character vector of the same length as `y` encoding which group each
  observation came from.

- sample_posterior_not_prior:

  A logical value: if this equals TRUE we sample from the posterior; if
  it equals FALSE we sample from the prior.

- cores:

  A positive integer: the number of cores to use to run MCMC chains in
  parallel.

- report_stan_progress:

  A logical value: if this equals TRUE we report the progress of the
  stan calculation; if it equals FALSE we suppress this reporting. An
  unsolved bug means that if `cores > 1`, progress will be reported even
  if `report_stan_progress = FALSE`.

- ...:

  Extra parameters are passed to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html).

## Value

A data.table with one row per param and one column per sample from the
joint probability distribution (posterior or prior).
