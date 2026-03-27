# Class: `distribution.discrete.binomial.class`

Derived class for an binomially-distributed random variable.

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.discrete.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)
-\> `distribution.discrete.binomial.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  The mean of a binomial distribution with size `$params$size` and
  success probability `$params$prob`.

- `sd`:

  The standard deviation of a binomial distribution with size
  `$params$size` and success probability `$params$prob`.

- `var`:

  The variance of a binomial distribution with size `$params$size` and
  success probability `$params$prob`.

## Methods

### Public methods

- [`distribution.discrete.binomial.class$new()`](#method-distribution.discrete.binomial.class-new)

- [`distribution.discrete.binomial.class$d()`](#method-distribution.discrete.binomial.class-d)

- [`distribution.discrete.binomial.class$p()`](#method-distribution.discrete.binomial.class-p)

- [`distribution.discrete.binomial.class$q()`](#method-distribution.discrete.binomial.class-q)

- [`distribution.discrete.binomial.class$r()`](#method-distribution.discrete.binomial.class-r)

- [`distribution.discrete.binomial.class$clone()`](#method-distribution.discrete.binomial.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.discrete.class`

#### Usage

    distribution.discrete.binomial.class$new(size, prob)

#### Arguments

- `size`:

  number of trials (zero or more).

- `prob`:

  probability of success on each trial.

------------------------------------------------------------------------

### Method `d()`

Density function for a binomial random variable with size `params$size`
and success probability `params$prob`.

#### Usage

    distribution.discrete.binomial.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a binomial random variable with size
`params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.binomial.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a binomial random variable with size `params$size`
and success probability `params$prob`.

#### Usage

    distribution.discrete.binomial.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a binomial random variable with size
`params$size` and success probability `params$prob`.

#### Usage

    distribution.discrete.binomial.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.discrete.binomial.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
