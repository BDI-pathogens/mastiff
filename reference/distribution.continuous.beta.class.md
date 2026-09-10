# Class: `distribution.continuous.beta.class`

Derived class for a beta random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.continuous.beta.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  the mean of the distribution

- `sd`:

  the standard deviation of the distribution

- `var`:

  the variance of the distribution

## Methods

### Public methods

- [`distribution.continuous.beta.class$new()`](#method-distribution.continuous.beta.class-new)

- [`distribution.continuous.beta.class$d()`](#method-distribution.continuous.beta.class-d)

- [`distribution.continuous.beta.class$p()`](#method-distribution.continuous.beta.class-p)

- [`distribution.continuous.beta.class$q()`](#method-distribution.continuous.beta.class-q)

- [`distribution.continuous.beta.class$r()`](#method-distribution.continuous.beta.class-r)

- [`distribution.continuous.beta.class$clone()`](#method-distribution.continuous.beta.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.normal.class`

#### Usage

    distribution.continuous.beta.class$new(alpha, beta)

#### Arguments

- `alpha`:

  the alpha shape parameter of a beta distribution

- `beta`:

  the beta shape parameter of a beta distribution

------------------------------------------------------------------------

### Method `d()`

density function for a beta random variable with shape parameters
`$params$alpha` and `$params$beta`

#### Usage

    distribution.continuous.beta.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a beta random variable with shape
parameters `$params$alpha` and `$params$beta`

#### Usage

    distribution.continuous.beta.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a beta random variable with shape parameters
`$params$alpha` and `$params$beta`

#### Usage

    distribution.continuous.beta.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a beta random variable with shape
parameters `$params$alpha` and `$params$beta`

#### Usage

    distribution.continuous.beta.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.beta.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
