# Class: `distribution.continuous.exponential.class`

Derived class for an exponentially-distributed random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.continuous.exponential.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  The mean of an exponential distribution with rate `$params$rate`.

- `sd`:

  The standard deviation of an exponential distribution with rate
  `$params$rate`.

- `var`:

  The variance of an exponential distribution with rate `$params$rate`.

- `offset`:

  The amount by which the exponential distribution is shifted. Creates a
  random variable `X~offset`+Exp(`params$rate`)

## Methods

### Public methods

- [`distribution.continuous.exponential.class$new()`](#method-distribution.continuous.exponential.class-new)

- [`distribution.continuous.exponential.class$d()`](#method-distribution.continuous.exponential.class-d)

- [`distribution.continuous.exponential.class$p()`](#method-distribution.continuous.exponential.class-p)

- [`distribution.continuous.exponential.class$q()`](#method-distribution.continuous.exponential.class-q)

- [`distribution.continuous.exponential.class$r()`](#method-distribution.continuous.exponential.class-r)

- [`distribution.continuous.exponential.class$clone()`](#method-distribution.continuous.exponential.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.exponential.class`

#### Usage

    distribution.continuous.exponential.class$new(rate = 1, offset = 0)

#### Arguments

- `rate`:

  The rate of the exponential distribution

- `offset`:

  The amount by which the exponential distribution is shifted. Creates a
  random variable `X~offset`+Exp(`params$rate`)

------------------------------------------------------------------------

### Method `d()`

Density function for an exponential random variable with rate
`params$rate`.

#### Usage

    distribution.continuous.exponential.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for an exponential random variable with rate
`params$rate`.

#### Usage

    distribution.continuous.exponential.class$p(
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

Quantile function for an exponential random variable with rate
`params$rate`.

#### Usage

    distribution.continuous.exponential.class$q(
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

Generates random deviates for an exponential random variable with rate
`params$rate`.

#### Usage

    distribution.continuous.exponential.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.exponential.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
