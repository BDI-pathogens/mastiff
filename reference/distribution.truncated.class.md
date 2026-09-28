# Class: `distribution.truncated.class`

Base class for truncated distributions

## Super classes

`R6.class.class` -\>
[`distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\> `distribution.truncated.class`

## Active bindings

- `distribution`:

  the untruncated distribution

- `t0`:

  the lower bound for truncation. The distribution is truncated to have
  support `[t0, t1]`

- `t1`:

  the upper bound for truncation. The distribution is truncated to have
  support `[t0, t1]`

- `support`:

  The support of the truncated distribution, i.e. the subset of values
  for which the density is positives

- `params`:

  Named list of distribution parameters

- `interfaces`:

  The list of available class interfaces

## Methods

### Public methods

- [`distribution.truncated.class$new()`](#method-distribution.truncated.class-initialize)

- [`distribution.truncated.class$d()`](#method-distribution.truncated.class-d)

- [`distribution.truncated.class$p()`](#method-distribution.truncated.class-p)

- [`distribution.truncated.class$q()`](#method-distribution.truncated.class-q)

- [`distribution.truncated.class$r()`](#method-distribution.truncated.class-r)

- [`distribution.truncated.class$clone()`](#method-distribution.truncated.class-clone)

------------------------------------------------------------------------

### `distribution.truncated.class$new()`

Create a new object of class `distribution.multivariate.class`

#### Usage

    distribution.truncated.class$new(
      distribution,
      t0 = distribution$support[1],
      t1 = distribution$support[2]
    )

#### Arguments

- `distribution`:

  the untruncated distribution

- `t0`:

  Lower bound for truncation

- `t1`:

  Upper bound for truncation

------------------------------------------------------------------------

### `distribution.truncated.class$d()`

Density function for a truncated random variable.

#### Usage

    distribution.truncated.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### `distribution.truncated.class$p()`

Cumulative density function for a truncated random variable.

#### Usage

    distribution.truncated.class$p(q, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `q`:

  vector of quantiles.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### `distribution.truncated.class$q()`

Quantile function for a truncated random variable.

#### Usage

    distribution.truncated.class$q(p, lower.tail = TRUE, log.p = FALSE)

#### Arguments

- `p`:

  vector of probabilities.

- `lower.tail`:

  logical; if TRUE (default), probabilities are \\P\[ X \leq x \]\\,
  otherwise, \\P\[X\>x\]\\.

- `log.p`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### `distribution.truncated.class$r()`

Generates random deviates for a truncated random variable by inversion
sampling from the truncated CDF.

#### Usage

    distribution.truncated.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### `distribution.truncated.class$clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.truncated.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
