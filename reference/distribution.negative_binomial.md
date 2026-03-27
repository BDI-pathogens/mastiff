# distribution.negative_binomial

Constructor function for an object of class
\[[distribution.discrete.negative_binomial.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.negative_binomial.class.md)\]

## Usage

``` r
distribution.negative_binomial(size, prob, mu)
```

## Arguments

- size:

  target for number of successful trials, or dispersion parameter (the
  shape parameter of the gamma mixing distribution). Must be strictly
  positive, need not be integer.

- prob:

  probability of success in each trial. 0 \< prob \<= 1.

- mu:

  alternative parametrization via mean: see
  [stats::dnbinom](https://rdrr.io/r/stats/NegBinomial.html)

## Value

An object of class
\[[distribution.discrete.negative_binomial.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.negative_binomial.class.md)\]

## See also

[Mastiff-Distributions](https://bdi-pathogens.github.io/mastiff/reference/Mastiff-Distributions.md)
