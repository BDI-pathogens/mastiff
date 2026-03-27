# Class: `distribution.abstract.class`

Base class for derived distributions

## Super class

`mastiff::R6.class.class` -\> `distribution.abstract.class`

## Active bindings

- `name`:

  The name of the distribution

- `param_names`:

  The names of all distribution parameters

- `params`:

  Named list of distribution parameters

- `interfaces`:

  The list of available class interfaces

- `mean`:

  Mean of the distribution

- `sd`:

  Standard deviation of the distribution

- `var`:

  Variance of the distribution

## Methods

### Public methods

- [`distribution.abstract.class$new()`](#method-distribution.abstract.class-new)

- [`distribution.abstract.class$d()`](#method-distribution.abstract.class-d)

- [`distribution.abstract.class$p()`](#method-distribution.abstract.class-p)

- [`distribution.abstract.class$q()`](#method-distribution.abstract.class-q)

- [`distribution.abstract.class$r()`](#method-distribution.abstract.class-r)

- [`distribution.abstract.class$clone()`](#method-distribution.abstract.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.abstract.class`

#### Usage

    distribution.abstract.class$new()

------------------------------------------------------------------------

### Method `d()`

Template base class function for density function of a distribution

#### Usage

    distribution.abstract.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Template base class function for cumulative distribution function

#### Usage

    distribution.abstract.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Template base class function for quantile function of a distribution

#### Usage

    distribution.abstract.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Template base class function for sampling random variates

#### Usage

    distribution.abstract.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.abstract.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
