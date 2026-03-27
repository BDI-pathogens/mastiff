# Class: `distribution.continuous.lognormal.class`

Derived class for a lognormally-distributed random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.continuous.lognormal.class`

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

- [`distribution.continuous.lognormal.class$new()`](#method-distribution.continuous.lognormal.class-new)

- [`distribution.continuous.lognormal.class$d()`](#method-distribution.continuous.lognormal.class-d)

- [`distribution.continuous.lognormal.class$p()`](#method-distribution.continuous.lognormal.class-p)

- [`distribution.continuous.lognormal.class$q()`](#method-distribution.continuous.lognormal.class-q)

- [`distribution.continuous.lognormal.class$r()`](#method-distribution.continuous.lognormal.class-r)

- [`distribution.continuous.lognormal.class$clone()`](#method-distribution.continuous.lognormal.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.normal.class`

#### Usage

    distribution.continuous.lognormal.class$new(meanlog, sdlog)

#### Arguments

- `meanlog`:

  the mean of log(X)

- `sdlog`:

  the standard deviation of log(X)

------------------------------------------------------------------------

### Method `d()`

Density function for a lognormal random variable with mean log(X)
`$params$meanlog` and standard deviation log(X) `$params$sdlog`.

#### Usage

    distribution.continuous.lognormal.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a lognormal random variable with mean
log(X) `$params$meanlog` and standard deviation log(X) `$params$sdlog`.

#### Usage

    distribution.continuous.lognormal.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a lognormal random variable with mean log(X)
`$params$meanlog` and standard deviation log(X) `$params$sdlog`.

#### Usage

    distribution.continuous.lognormal.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a lognormal random variable with mean
log(X) `$params$meanlog` and standard deviation logx(X) `$params$sdlog`
.

#### Usage

    distribution.continuous.lognormal.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.lognormal.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
