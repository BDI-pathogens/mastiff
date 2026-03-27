# Class: `distribution.discrete.point_mass.class`

Derived class for a point mass at `$params$value`

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.discrete.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)
-\> `distribution.discrete.point_mass.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  The mean of a point mass at `$params$value`.

- `sd`:

  The standard deviation of a point mass at `$params$value`.

- `var`:

  The variance of a point mass at `$params$value`.

## Methods

### Public methods

- [`distribution.discrete.point_mass.class$new()`](#method-distribution.discrete.point_mass.class-new)

- [`distribution.discrete.point_mass.class$d()`](#method-distribution.discrete.point_mass.class-d)

- [`distribution.discrete.point_mass.class$p()`](#method-distribution.discrete.point_mass.class-p)

- [`distribution.discrete.point_mass.class$q()`](#method-distribution.discrete.point_mass.class-q)

- [`distribution.discrete.point_mass.class$r()`](#method-distribution.discrete.point_mass.class-r)

- [`distribution.discrete.point_mass.class$clone()`](#method-distribution.discrete.point_mass.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.discrete.class`

#### Usage

    distribution.discrete.point_mass.class$new(value)

#### Arguments

- `value`:

  The point with mass 1.

------------------------------------------------------------------------

### Method `d()`

Density function for a point mass at `params$value`.

#### Usage

    distribution.discrete.point_mass.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a point mass at `params$value`.

#### Usage

    distribution.discrete.point_mass.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a point mass at `params$value`.

#### Usage

    distribution.discrete.point_mass.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a point mass at `params$value`.

#### Usage

    distribution.discrete.point_mass.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.discrete.point_mass.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
