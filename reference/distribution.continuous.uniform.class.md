# Class: `distribution.continuous.uniform.class`

Derived class for an uniformly-distributed random variable on
`[min, max]`

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.continuous.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)
-\> `distribution.continuous.uniform.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces

- `mean`:

  The mean of a uniform random variable on `[min, max]`.

- `sd`:

  The standard deviation of a uniform random variable on `[min, max]`.

- `var`:

  The variance of a uniform random variable on `[min, max]`.

## Methods

### Public methods

- [`distribution.continuous.uniform.class$new()`](#method-distribution.continuous.uniform.class-new)

- [`distribution.continuous.uniform.class$d()`](#method-distribution.continuous.uniform.class-d)

- [`distribution.continuous.uniform.class$p()`](#method-distribution.continuous.uniform.class-p)

- [`distribution.continuous.uniform.class$q()`](#method-distribution.continuous.uniform.class-q)

- [`distribution.continuous.uniform.class$r()`](#method-distribution.continuous.uniform.class-r)

- [`distribution.continuous.uniform.class$clone()`](#method-distribution.continuous.uniform.class-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.continuous.exponential.class`

#### Usage

    distribution.continuous.uniform.class$new(min = 0, max = 1)

#### Arguments

- `min`:

  The lower bound of the uniform distribution

- `max`:

  The max bound of the uniform distribution

------------------------------------------------------------------------

### Method `d()`

Density function for a uniform random variable on `[min, max]`.

#### Usage

    distribution.continuous.uniform.class$d(x, log = FALSE)

#### Arguments

- `x`:

  vector of quantiles.

- `log`:

  logical; if TRUE, probabilities p are given as `log(p)`.

------------------------------------------------------------------------

### Method `p()`

Cumulative density function for a uniform random variable on
`[min, max]`.

#### Usage

    distribution.continuous.uniform.class$p(q, lower.tail = TRUE, log.p = FALSE)

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

Quantile function for a uniform random variable on `[min, max]`. rate
`params$rate`.

#### Usage

    distribution.continuous.uniform.class$q(p, lower.tail = TRUE, log.p = FALSE)

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

Generates random deviates for a uniform random variable on `[min, max]`.

#### Usage

    distribution.continuous.uniform.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.continuous.uniform.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
