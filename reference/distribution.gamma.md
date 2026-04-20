# distribution.gamma

Constructor function for an object of class
`distribution.continuous.gamma.class`

## Usage

``` r
distribution.gamma(shape, rate, scale, offset = 0)
```

## Arguments

- shape:

  The shape of the gamma distribution

- rate:

  The rate of the gamma distribution

- scale:

  an alternative way to specify the rate

- offset:

  offset The amount by which the gamma distribution is shifted. Creates
  a random variable `X~offset`+Gamma(`params$shape`, `params$rate`)

## Value

An object of class
\[[distribution.continuous.gamma.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.gamma.class.md)\]

## See also

[Mastiff-Distributions](https://bdi-pathogens.github.io/mastiff/reference/Mastiff-Distributions.md)
