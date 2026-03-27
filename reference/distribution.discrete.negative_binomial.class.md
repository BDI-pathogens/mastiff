# Class: `distribution.discrete.negative_binomial.class`

Derived class for an negative binomially-distributed random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.discrete.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)
-\> `distribution.discrete.negative_binomial.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `params`:

  Named list of distribution parameters

- `mean`:

  The mean of a negative_binomial distribution with mean
  `$params$lambda`.

- `sd`:

  The standard deviation of a negative_binomial distribution with mean
  `$params$lambda`.

- `var`:

  The variance of a negative_binomial distribution with mean
  `$params$lambda`.

## Methods

### Public methods

- [`distribution.discrete.negative_binomial.class$new()`](#method-distribution.discrete.negative_binomial.class-new)

- [`distribution.discrete.negative_binomial.class$d()`](#method-distribution.discrete.negative_binomial.class-d)

- [`distribution.discrete.negative_binomial.class$p()`](#method-distribution.discrete.negative_binomial.class-p)

- [`distribution.discrete.negative_binomial.class$q()`](#method-distribution.discrete.negative_binomial.class-q)

- [`distribution.discrete.negative_binomial.class$r()`](#method-distribution.discrete.negative_binomial.class-r)

- [`distribution.discrete.negative_binomial.class$clone()`](#method-distribution.discrete.negative_binomial.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class
`distribution.discrete.negative_binomial.class`

#### Usage

    distribution.discrete.negative_binomial.class$new(size, prob, mu)

#### Arguments

- `size`:

  target for number of successful trials, or dispersion parameter (the
  shape parameter of the gamma mixing distribution). Must be strictly
  positive, need not be integer.

- `prob`:

  probability of success in each trial. 0 \< prob \<= 1.

- `mu`:

  alternative parametrization via mean: see
  [stats::dnbinom](https://rdrr.io/r/stats/NegBinomial.html)

------------------------------------------------------------------------

### Method `d()`

Density function for a negative_binomial random variable with size
`params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.negative_binomial.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a negative_binomial random variable with
size `params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.negative_binomial.class$p(
      q,
      lower.tail = TRUE,
      log.p = FALSE
    )

#### Arguments

- `q`:

  vector of quantiles.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method [`q()`](https://rdrr.io/r/base/quit.html)

Quantile function for a negative_binomial random variable with size
`params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.negative_binomial.class$q(
      p,
      lower.tail = TRUE,
      log.p = FALSE
    )

#### Arguments

- `p`:

  vector of probabilities.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `r()`

Generates random deviates for a negative_binomial random variable with
size `params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.negative_binomial.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.discrete.negative_binomial.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
