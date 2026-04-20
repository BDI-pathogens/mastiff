# Class: `distribution.continuous.gamma.class`

Derived class for a gamma-distributed random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.continuous.gamma.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `params`:

  Named list of distribution parameters

- `mean`:

  The mean of a gamma distribution with shape `$params$shape` and rate
  `$params$rate`.

- `sd`:

  The standard deviation of a gamma distribution with shape
  `$params$shape` and rate `$params$rate`.

- `var`:

  The variance of a gamma distribution with shape `$params$shape` and
  rate `$params$rate`.

- `offset`:

  The amount by which the gamma distribution is shifted. Creates a
  random variable `X~offset`+Gamma(`params$shape`, `params$rate`)

## Methods

### Public methods

- [`distribution.continuous.gamma.class$new()`](#method-distribution.continuous.gamma.class-new)

- [`distribution.continuous.gamma.class$d()`](#method-distribution.continuous.gamma.class-d)

- [`distribution.continuous.gamma.class$p()`](#method-distribution.continuous.gamma.class-p)

- [`distribution.continuous.gamma.class$q()`](#method-distribution.continuous.gamma.class-q)

- [`distribution.continuous.gamma.class$r()`](#method-distribution.continuous.gamma.class-r)

- [`distribution.continuous.gamma.class$clone()`](#method-distribution.continuous.gamma.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.gamma.class`

#### Usage

    distribution.continuous.gamma.class$new(shape, rate, scale, offset = 0)

#### Arguments

- `shape`:

  The shape of the gamma distribution

- `rate`:

  The rate of the gamma distribution

- `scale`:

  an alternative way to specify the rate

- `offset`:

  offset The amount by which the gamma distribution is shifted. Creates
  a random variable `X~offset`+Gamma(`params$shape`, `params$rate`)

------------------------------------------------------------------------

### Method `d()`

Density function for a gamma random variable with rate `params$rate`.

#### Usage

    distribution.continuous.gamma.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a gamma random variable with rate
`params$rate`.

#### Usage

    distribution.continuous.gamma.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a gamma random variable with rate `params$rate`.

#### Usage

    distribution.continuous.gamma.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a gamma random variable with rate
`params$rate`.

#### Usage

    distribution.continuous.gamma.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.gamma.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
