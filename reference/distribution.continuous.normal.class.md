# Class: `distribution.continuous.normal.class`

Derived class for a normally-distributed random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.continuous.normal.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  The mean of a normal distribution with rate `$params$rate`.

- `sd`:

  The standard deviation of a normal distribution with rate
  `$params$rate`.

- `var`:

  The variance of a normal distribution with rate `$params$rate`.

## Methods

### Public methods

- [`distribution.continuous.normal.class$new()`](#method-distribution.continuous.normal.class-new)

- [`distribution.continuous.normal.class$d()`](#method-distribution.continuous.normal.class-d)

- [`distribution.continuous.normal.class$p()`](#method-distribution.continuous.normal.class-p)

- [`distribution.continuous.normal.class$q()`](#method-distribution.continuous.normal.class-q)

- [`distribution.continuous.normal.class$r()`](#method-distribution.continuous.normal.class-r)

- [`distribution.continuous.normal.class$clone()`](#method-distribution.continuous.normal.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.normal.class`

#### Usage

    distribution.continuous.normal.class$new(mean, sd)

#### Arguments

- `mean`:

  The mean of the normal distribution.

- `sd`:

  The standard deviation of the normal distribution.

------------------------------------------------------------------------

### Method `d()`

Density function for a normal random variable with mean `$params$mean`
and standard deviation `$params$sd`.

#### Usage

    distribution.continuous.normal.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a normal random variable with mean
`$params$mean` and standard deviation `$params$sd`.

#### Usage

    distribution.continuous.normal.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a normal random variable with mean `$params$mean`
and standard deviation `$params$sd`.

#### Usage

    distribution.continuous.normal.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a normal random variable with mean
`$params$mean` and standard deviation `$params$sd`.

#### Usage

    distribution.continuous.normal.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.normal.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
