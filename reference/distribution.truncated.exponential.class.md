# Class: `distribution.truncated.exponential.class`

Derived class for an exponentially-distributed random variable with
interval truncation.

## Super classes

`R6.class.class` -\>
[`distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`distribution.truncated.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.md)
-\> `distribution.truncated.exponential.class`

## Active bindings

- `mean`:

  The mean of an exponential distribution with rate `$params$rate`
  truncated to `[$t0, $t1]`.

- `sd`:

  The standard deviation of an exponential distribution with rate
  `$params$rate` truncated to `[$t0, $t1]`.

- `var`:

  The variance of an exponential distribution with rate `$params$rate`
  truncated to `[$t0, $t1]`.

- `interfaces`:

  The list of available class interfaces

## Methods

### Public methods

- [`distribution.truncated.exponential.class$new()`](#method-distribution.truncated.exponential.class-initialize)

- [`distribution.truncated.exponential.class$clone()`](#method-distribution.truncated.exponential.class-clone)

Inherited methods

- [`distribution.truncated.class$d()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-d)
- [`distribution.truncated.class$p()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-p)
- [`distribution.truncated.class$q()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-q)
- [`distribution.truncated.class$r()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-r)

------------------------------------------------------------------------

### `distribution.truncated.exponential.class$new()`

Create a new object of class `distribution.truncated.exponential.class`

#### Usage

    distribution.truncated.exponential.class$new(rate = 1, t0 = 0, t1 = Inf)

#### Arguments

- `rate`:

  The rate of the exponential distribution

- `t0`:

  Lower bound for truncation

- `t1`:

  Upper bound for truncation

------------------------------------------------------------------------

### `distribution.truncated.exponential.class$clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.truncated.exponential.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
