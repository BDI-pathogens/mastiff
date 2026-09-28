# distribution.truncated

Constructor function for an object of class
\[[distribution.truncated.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.md)\]

## Usage

``` r
distribution.truncated(
  distribution,
  t0 = distribution$support[1],
  t1 = distribution$support[2]
)
```

## Arguments

- distribution:

  the untruncated distribution

- t0, t1:

  Upper and lower bound for truncation

## Value

An object of class
\[[distribution.truncated.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.truncated.class.md)\]
