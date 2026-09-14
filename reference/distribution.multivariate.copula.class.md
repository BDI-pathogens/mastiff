# Class: `distribution.multivariate.copual.class`

Class for copula distributions

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`mastiff::distribution.multivariate.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.multivariate.class.md)
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

- [`distribution.multivariate.copula.class$new()`](#method-distribution.multivariate.copula.class-new)

- [`distribution.multivariate.copula.class$r()`](#method-distribution.multivariate.copula.class-r)

- [`distribution.multivariate.copula.class$clone()`](#method-distribution.multivariate.copula.class-clone)

Inherited methods

- [`mastiff::distribution.abstract.class$d()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-d)
- [`mastiff::distribution.abstract.class$p()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-p)
- [`mastiff::distribution.abstract.class$q()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-q)

------------------------------------------------------------------------

### Method `new()`

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

### Method `r()`

Generates random deviates of a multivariate normal with rate
`params$rate`.

#### Usage

    distribution.multivariate.copula.class$r(n)

#### Arguments

- `n`:

  number of observations. If `length( n ) > 1`, the length is taken to
  be the number required.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.multivariate.copula.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
