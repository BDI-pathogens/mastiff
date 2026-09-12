# Class: `distribution.multivariate.class`

Base class for multivariate distributions

## Super classes

`mastiff::R6.class.class` -\>
[`mastiff::distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\> `distribution.multivariate.class`

## Active bindings

- `interfaces`:

  The list of available class interfaces.

- `support`:

  The support of the continuous distribution, i.e. the subset of values
  for which the density is positive,

- `n_dimensions`:

  the number of dimensions of random variable

## Methods

### Public methods

- [`distribution.multivariate.class$new()`](#method-distribution.multivariate.class-new)

- [`distribution.multivariate.class$clone()`](#method-distribution.multivariate.class-clone)

Inherited methods

- [`mastiff::distribution.abstract.class$d()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-d)
- [`mastiff::distribution.abstract.class$p()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-p)
- [`mastiff::distribution.abstract.class$q()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-q)
- [`mastiff::distribution.abstract.class$r()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.html#method-r)

------------------------------------------------------------------------

### Method `new()`

Create a new object of class `distribution.multivariate.class`

#### Usage

    distribution.multivariate.class$new(n_dimensions)

#### Arguments

- `n_dimensions`:

  the number of dimensions of random variable

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.multivariate.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
