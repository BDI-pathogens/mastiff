# Class: `distribution.multivariate.t.class`

Base class for multivariate t distribution

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.multivariate.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.multivariate.class.md)
-\> `distribution.multivariate.t.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces.

- `mean`:

  the means of each variable

- `sd`:

  the standard deviation of each variable

- `var`:

  the variance of variable

## Methods

### Public methods

- [`distribution.multivariate.t.class$new()`](#method-distribution.multivariate.t.class-new)

- [`distribution.multivariate.t.class$set_uniform_correlation()`](#method-distribution.multivariate.t.class-set_uniform_correlation)

- [`distribution.multivariate.t.class$d()`](#method-distribution.multivariate.t.class-d)

- [`distribution.multivariate.t.class$r()`](#method-distribution.multivariate.t.class-r)

- [`distribution.multivariate.t.class$p()`](#method-distribution.multivariate.t.class-p)

- [`distribution.multivariate.t.class$q()`](#method-distribution.multivariate.t.class-q)

- [`distribution.multivariate.t.class$clone()`](#method-distribution.multivariate.t.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.multivariate.normal.class`

#### Usage

    distribution.multivariate.t.class$new(means, scale_matrix, df)

#### Arguments

- `means`:

  vector of means

- `scale_matrix`:

  the scale_matrix matrix

- `df`:

  the degrees of freedom

------------------------------------------------------------------------

### Method `set_uniform_correlation()`

Updates to the scale_matrix matrix so that all off-diagonal correlations
are the same

#### Usage

    distribution.multivariate.t.class$set_uniform_correlation(rho)

#### Arguments

- `rho`:

  the single correlation between all variables

------------------------------------------------------------------------

### Method `d()`

Density function for a multivariate normal

#### Usage

    distribution.multivariate.t.class$d(x, log = FALSE)

#### Arguments

- `x`:

  matrix of varlues

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `r()`

Generates random deviates of a multivariate normal with rate
`params$rate`.

#### Usage

    distribution.multivariate.t.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `p()`

Univariate cumulative density functions

#### Usage

    distribution.multivariate.t.class$p(q, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `q`:

  matrix of univariate quantiles.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method [`q()`](https://rdrr.io/r/base/quit.html)

Univariate quantile functions

#### Usage

    distribution.multivariate.t.class$q(p, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `p`:

  matrix of univariate probabilities.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.multivariate.t.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
