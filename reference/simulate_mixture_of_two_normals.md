# Simulate (randomly draw) numbers from a mixture of two normal distributions.

Simulate (randomly draw) numbers from a mixture of two normal
distributions.

## Usage

``` r
simulate_mixture_of_two_normals(
  n = 500,
  groups = LETTERS[1:5],
  group_frequencies = rep(1/length(groups), length(groups)),
  mu_0 = 0,
  mu_1 = mu_0 + 3,
  sd_0 = 0.5,
  sd_1 = 1,
  p = 0.5,
  sd_groups = 1
)
```

## Arguments

- n:

  A non-negative integer: the number of observations to simulate.

- groups:

  A character vector: a set of categories that differ in their
  proportions for the mixture. Specify `character(0)` to have all
  observations in the same category, i.e. all observations have the same
  probability of coming from one component of the mixture or the other.

- group_frequencies:

  A numeric vector of the same length as `groups`, each value being the
  frequency of that group (as a proportion between 0 and 1).

- mu_0:

  A number: the smaller of the means of the two normals.

- mu_1:

  A number: the larger of the means of the two normals.

- sd_0:

  A non-negative number: the standard deviation of the normal with mean
  mu_0.

- sd_1:

  A non-negative number: the standard deviation of the normal with mean
  mu_1.

- p:

  The probability of an observation coming from the normal with mean
  mu_1.

- sd_groups:

  A non-negative number: the standard deviation of the normal
  variability between regression coefficients for the groups (on a logit
  scale).

## Value

A dataframe with `n` rows, one per observation. The column `y` contains
the values drawn from the normal mixture. The column `d` contains a
logical variable for whether that observation came from the normal with
mean `mu_1`.

## Examples

``` r
df <- simulate_mixture_of_two_normals()$data
hist(df$y)

hist(df[df$d, ]$y)   # the normal with mean mu_1

hist(df[! df$d, ]$y) # the normal with mean mu_0
```
