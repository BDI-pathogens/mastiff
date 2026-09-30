# Class: `distribution.multivariate.copual.class`

Class for copula distributions

## Super classes

`R6.class.class` -\>
[`distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`distribution.multivariate.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.multivariate.class.md)
-\> `distribution.multivariate.copula.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces.

- `mean`:

  the means of each variable

- `sd`:

  the standard deviation of each variable

- `var`:

  the variance of variable

- `distributions`:

  the univariate distributions in the copula

- `copula`:

  the multivariate copula distribution generating the correlation
  between random variables

## Methods

### Public methods

- [`distribution.multivariate.copula.class$new()`](#method-distribution.multivariate.copula.class-initialize)

- [`distribution.multivariate.copula.class$r()`](#method-distribution.multivariate.copula.class-r)

- [`distribution.multivariate.copula.class$clone()`](#method-distribution.multivariate.copula.class-clone)

Inherited methods

- [`distribution.abstract.class$d()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-d)
- [`distribution.abstract.class$p()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-p)
- [`distribution.abstract.class$q()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-q)

------------------------------------------------------------------------

### `distribution.multivariate.copula.class$new()`

Create a new object of class `distribution.multivaraite.copula.class`

#### Usage

    distribution.multivariate.copula.class$new(distributions, copula)

#### Arguments

- `distributions`:

  the univariate distributions in the copula

- `copula`:

  the multivariate copula distribution generating the correlation
  between random variables

------------------------------------------------------------------------

### `distribution.multivariate.copula.class$r()`

Generates random deviates of a multivariate normal with rate
`params$rate`.

#### Usage

    distribution.multivariate.copula.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### `distribution.multivariate.copula.class$clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.multivariate.copula.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
