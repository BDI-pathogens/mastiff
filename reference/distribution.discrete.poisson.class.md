# Class: `distribution.discrete.poisson.class`

Derived class for an Poisson-distributed random variable.

## Super classes

`R6.class.class` -\>
[`distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`distribution.discrete.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)
-\> `distribution.discrete.poisson.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  The mean of a Poisson distribution with mean `$params$lambda`.

- `sd`:

  The standard deviation of a Poisson distribution with mean
  `$params$lambda`.

- `var`:

  The variance of a Poisson distribution with mean `$params$lambda`.

## Methods

### Public methods

- [`distribution.discrete.poisson.class$new()`](#method-distribution.discrete.poisson.class-initialize)

- [`distribution.discrete.poisson.class$d()`](#method-distribution.discrete.poisson.class-d)

- [`distribution.discrete.poisson.class$p()`](#method-distribution.discrete.poisson.class-p)

- [`distribution.discrete.poisson.class$q()`](#method-distribution.discrete.poisson.class-q)

- [`distribution.discrete.poisson.class$r()`](#method-distribution.discrete.poisson.class-r)

- [`distribution.discrete.poisson.class$clone()`](#method-distribution.discrete.poisson.class-clone)

------------------------------------------------------------------------

### `distribution.discrete.poisson.class$new()`

Create a new object of class `distribution.discrete.class`

#### Usage

    distribution.discrete.poisson.class$new(lambda)

#### Arguments

- `lambda`:

  vector of (non-negative) means.

------------------------------------------------------------------------

### `distribution.discrete.poisson.class$d()`

Density function for a poisson random variable with size `params$size`
and success probability `params$prob`.

#### Usage

    distribution.discrete.poisson.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### `distribution.discrete.poisson.class$p()`

Cumulative density function for a poisson random variable with size
`params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.poisson.class$p(q, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `q`:

  vector of quantiles.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### `distribution.discrete.poisson.class$q()`

Quantile function for a poisson random variable with size `params$size`
and success probability `params$prob`.

#### Usage

    distribution.discrete.poisson.class$q(p, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `p`:

  vector of probabilities.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### `distribution.discrete.poisson.class$r()`

Generates random deviates for a poisson random variable with size
`params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.poisson.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### `distribution.discrete.poisson.class$clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.discrete.poisson.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
