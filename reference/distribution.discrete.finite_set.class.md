# Class: `distribution.discrete.finite_set.class`

Derived class for a generic discrete distribution on a finite set of
points in (-Inf, Inf).

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.discrete.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)
-\> `distribution.discrete.finite_set.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `params`:

  Named list of distribution parameters

- `support`:

  The support of the distribution, i.e. the subset of values for which
  the density is positive,

- `mean`:

  The mean of a point mass at `$params$value`.

- `sd`:

  The standard deviation of a point mass at `$params$value`.

- `var`:

  The variance of a point mass at `$params$value`.

## Methods

### Public methods

- [`distribution.discrete.finite_set.class$new()`](#method-distribution.discrete.finite_set.class-new)

- [`distribution.discrete.finite_set.class$d()`](#method-distribution.discrete.finite_set.class-d)

- [`distribution.discrete.finite_set.class$p()`](#method-distribution.discrete.finite_set.class-p)

- [`distribution.discrete.finite_set.class$q()`](#method-distribution.discrete.finite_set.class-q)

- [`distribution.discrete.finite_set.class$r()`](#method-distribution.discrete.finite_set.class-r)

- [`distribution.discrete.finite_set.class$clone()`](#method-distribution.discrete.finite_set.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.discrete.class`

#### Usage

    distribution.discrete.finite_set.class$new(support, prob)

#### Arguments

- `support`:

  vector of values giving the points in the finite set on which the
  distribution has support.

- `prob`:

  vector of probability weights for obtaining each element of support.

------------------------------------------------------------------------

### Method `d()`

Density function for the probability distribution with mass
`$params$prob` at points `$support`.

#### Usage

    distribution.discrete.finite_set.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for the probability distribution with mass
`$params$prob` at points `$support`.

#### Usage

    distribution.discrete.finite_set.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for the probability distribution with mass
`$params$prob` at points `$support`.

#### Usage

    distribution.discrete.finite_set.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for probability distribution with mass
`$params$prob` at points `$support`.

#### Usage

    distribution.discrete.finite_set.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.discrete.finite_set.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
