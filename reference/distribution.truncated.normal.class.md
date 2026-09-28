# Class: `distribution.truncated.normal.class`

Derived class for an normally-distributed random variable with interval
truncation.

## Super classes

`R6.class.class` -\>
[`distribution.abstract.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.abstract.class.md)
-\>
[`distribution.truncated.class`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.md)
-\> `distribution.truncated.normal.class`

## Active bindings

- `mean`:

  The mean of a normal distribution with mean `$params$mean` and
  standard deviation `$params$sd` truncated to `[$t0, $t1]`.

- `sd`:

  The standard deviation of a normal distribution with mean
  `$params$mean` and standard deviation `$params$sd` truncated to
  `[$t0, $t1]`.

- `var`:

  The variance of a normal distribution with mean `$params$mean` and
  standard deviation `$params$sd` truncated to `[$t0, $t1]`.

- `interfaces`:

  The list of available class interfaces

## Methods

### Public methods

- [`distribution.truncated.normal.class$new()`](#method-distribution.truncated.normal.class-initialize)

- [`distribution.truncated.normal.class$clone()`](#method-distribution.truncated.normal.class-clone)

Inherited methods

- [`distribution.truncated.class$d()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-d)
- [`distribution.truncated.class$p()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-p)
- [`distribution.truncated.class$q()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-q)
- [`distribution.truncated.class$r()`](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.html#method-r)

------------------------------------------------------------------------

### `distribution.truncated.normal.class$new()`

Create a new object of class `distribution.truncated.normal.class`

#### Usage

    distribution.truncated.normal.class$new(mean = 0, sd = 1, t0 = 0, t1 = Inf)

#### Arguments

- `mean`:

  The mean of the untruncated normal distribution.

- `sd`:

  The standard deviation of the untruncated normal distribution.

- `t0`:

  Lower bound for truncation

- `t1`:

  Upper bound for truncation

------------------------------------------------------------------------

### `distribution.truncated.normal.class$clone()`

The objects of this class are cloneable with this method.

#### Usage

    distribution.truncated.normal.class$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
