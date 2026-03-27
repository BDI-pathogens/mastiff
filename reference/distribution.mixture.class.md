# Class: `distribution.mixture.class`

Class to describe the mixture of distributions

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.mixture.class`

## Active bindings

- `n_distributions`:

  the number of distributions in the mixture

- `distributions`:

  the distributions in the mixture

- `weights`:

  the weights of the distributions in the mixture

- `interfaces`:

  the list of available class interfaces

## Methods

### Public methods

- [`distribution.mixture.class$new()`](#method-distribution.mixture.class-new)

- [`distribution.mixture.class$d()`](#method-distribution.mixture.class-d)

- [`distribution.mixture.class$p()`](#method-distribution.mixture.class-p)

- [`distribution.mixture.class$q()`](#method-distribution.mixture.class-q)

- [`distribution.mixture.class$r()`](#method-distribution.mixture.class-r)

- [`distribution.mixture.class$clone()`](#method-distribution.mixture.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.mixture.class`

#### Usage

    distribution.mixture.class$new(distributions, weights)

#### Arguments

- `distributions`:

  list of distributions in the mixture

- `weights`:

  vector of weights of the distributions in the mixture

------------------------------------------------------------------------

### Method `d()`

Density function for a random variable of the mixture

#### Usage

    distribution.mixture.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Evaluates the distribution function of the mixture

#### Usage

    distribution.mixture.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Evaluates the quantile function of the mixture

#### Usage

    distribution.mixture.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random samples of the mixture

#### Usage

    distribution.mixture.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.mixture.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
