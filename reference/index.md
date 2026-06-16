# Package index

- [`check_logical()`](https://bdi-pathogens.github.io/mastiff/reference/check_logical.md)
  : Checks a variable is a single logical value (and optionally not
  missing)

- [`check_numeric()`](https://bdi-pathogens.github.io/mastiff/reference/check_numeric.md)
  : Checks a variable is a single number (and optionally, in a range and
  not NA)

- [`estimate_mixture_of_two_normals()`](https://bdi-pathogens.github.io/mastiff/reference/estimate_mixture_of_two_normals.md)
  : Estimate the parameters of a mixture of two normal distributions

- [`logistic()`](https://bdi-pathogens.github.io/mastiff/reference/logistic.md)
  :

  logistic(x) = 1 / (1 + exp(-x)), a wrapper for
  [`stats::plogis()`](https://rdrr.io/r/stats/Logistic.html)

- [`logit()`](https://bdi-pathogens.github.io/mastiff/reference/logit.md)
  :

  logit(p) = log(p / (1 - p)), a wrapper for
  [`stats::qlogis()`](https://rdrr.io/r/stats/Logistic.html)

- [`plot_posterior()`](https://bdi-pathogens.github.io/mastiff/reference/plot_posterior.md)
  : Plots the marginal posteriors of each parameter in a stanfit object

- [`posterior_intervals()`](https://bdi-pathogens.github.io/mastiff/reference/posterior_intervals.md)
  : Calculates central probability intervals for each parameter for each
  stanfit object

- [`posterior_mass_in_range()`](https://bdi-pathogens.github.io/mastiff/reference/posterior_mass_in_range.md)
  : Calculates the amount of probability (mass) a parameter has in a
  range

- [`posterior_means()`](https://bdi-pathogens.github.io/mastiff/reference/posterior_means.md)
  [`posterior_medians()`](https://bdi-pathogens.github.io/mastiff/reference/posterior_means.md)
  : Calculates the mean or median for each parameter for each stanfit
  object

- [`rename_params_cmdstanfile_to_rstan()`](https://bdi-pathogens.github.io/mastiff/reference/rename_params_cmdstanfile_to_rstan.md)
  : Renames tensor parameters from cmdstandr to rstan format

- [`simulate_mixture_of_two_normals()`](https://bdi-pathogens.github.io/mastiff/reference/simulate_mixture_of_two_normals.md)
  : Simulate (randomly draw) numbers from a mixture of two normal
  distributions.

- [`stan_example_regression`](https://bdi-pathogens.github.io/mastiff/reference/stan_example_regression.md)
  : Example data from Stan analysis of simple linear normal regression

- [`stanfit_to_matrix()`](https://bdi-pathogens.github.io/mastiff/reference/stanfit_to_matrix.md)
  [`stanfit_to_dt()`](https://bdi-pathogens.github.io/mastiff/reference/stanfit_to_matrix.md)
  : Converts a stanfit object to a matrix or data.table

- [`uniroot.vectorized()`](https://bdi-pathogens.github.io/mastiff/reference/uniroot.vectorized.md)
  : Vectorised uniroot

## R6 Interface Classes

Class definitions for an extension to R6 classes including interfaces

- [`R6.class()`](https://bdi-pathogens.github.io/mastiff/reference/R6.class.md)
  : Class: R6.class
- [`R6.interface()`](https://bdi-pathogens.github.io/mastiff/reference/R6.interface.md)
  : R6.interface
- [`R6.interface.implements()`](https://bdi-pathogens.github.io/mastiff/reference/R6.interface.implements.md)
  : R6.interface.implements

## R6 Distribution Classes

Constructor functions for R6 distribution classes

- [`Mastiff-Distributions`](https://bdi-pathogens.github.io/mastiff/reference/Mastiff-Distributions.md)
  : Distribution Classes
- [`is.distribution()`](https://bdi-pathogens.github.io/mastiff/reference/is.distribution.md)
  : is.distribution
- [`distribution.mixture()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.mixture.md)
  : distribution.mixture

### Discrete Distributions

- [`distribution.binomial()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.binomial.md)
  : distribution.binomial
- [`distribution.finite_set()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.finite_set.md)
  : distribution.finite_set
- [`distribution.negative_binomial()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.negative_binomial.md)
  : distribution.negative_binomial
- [`distribution.point_mass()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.point_mass.md)
  : distribution.point_mass
- [`distribution.poisson()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.poisson.md)
  : distribution.poisson

### Continuous Distributions

- [`distribution.exponential()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.exponential.md)
  : distribution.exponential
- [`distribution.gamma()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.gamma.md)
  : distribution.gamma
- [`distribution.lognormal()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.lognormal.md)
  : distribution.lognormal
- [`distribution.normal()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.normal.md)
  : distribution.normal
- [`distribution.uniform()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.uniform.md)
  : distribution.uniform

## Class Definitions

- [`distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
  :

  Class: `distribution.abstract.class`

- [`distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
  :

  Class: `distribution.continuous.class`

- [`distribution.continuous.exponential.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.exponential.class.md)
  :

  Class: `distribution.continuous.exponential.class`

- [`distribution.continuous.gamma.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.gamma.class.md)
  :

  Class: `distribution.continuous.gamma.class`

- [`distribution.continuous.lognormal.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.lognormal.class.md)
  :

  Class: `distribution.continuous.lognormal.class`

- [`distribution.continuous.normal.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.normal.class.md)
  :

  Class: `distribution.continuous.normal.class`

- [`distribution.continuous.uniform.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.uniform.class.md)
  :

  Class: `distribution.continuous.uniform.class`

- [`distribution.discrete.binomial.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.binomial.class.md)
  :

  Class: `distribution.discrete.binomial.class`

- [`distribution.discrete.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)
  :

  Class: `distribution.discrete.class`

- [`distribution.discrete.finite_set.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.finite_set.class.md)
  :

  Class: `distribution.discrete.finite_set.class`

- [`distribution.discrete.negative_binomial.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.negative_binomial.class.md)
  :

  Class: `distribution.discrete.negative_binomial.class`

- [`distribution.discrete.point_mass.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.point_mass.class.md)
  :

  Class: `distribution.discrete.point_mass.class`

- [`distribution.discrete.poisson.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.poisson.class.md)
  :

  Class: `distribution.discrete.poisson.class`

- [`distribution.mixture.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.mixture.class.md)
  :

  Class: `distribution.mixture.class`
