# Class: `distribution.continuous.class`

Base class for univariate continuous distributions

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\> `distribution.continuous.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces.

- `support`:

  The support of the continuous distribution, i.e. the subset of values
  for which the density is positive,

## Methods

### Public methods

- [`distribution.continuous.class$new()`](#method-distribution.continuous.class-new)

- [`distribution.continuous.class$p()`](#method-distribution.continuous.class-p)

- [`distribution.continuous.class$q()`](#method-distribution.continuous.class-q)

- [`distribution.continuous.class$clone()`](#method-distribution.continuous.class-clone)

Inherited methods

- [`mastiff::distribution.abstract.class$d()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-d)
- [`mastiff::distribution.abstract.class$r()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-r)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.class`

#### Usage

    distribution.continuous.class$new(support = c(-Inf, Inf))

#### Arguments

- `support`:

  The support of the distribution, i.e. the subset of values for which
  the density is positive.

------------------------------------------------------------------------

### Method `p()`

Evaluates the distribution function of a discrete random variable with
finite integer support given density function `$d()`

#### Usage

    distribution.continuous.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Evaluates the distribution function of a discrete random variable with
finite integer support given distribution function `$p()`

#### Usage

    distribution.continuous.class$q(p, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `p`:

  vector of probabilities.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
