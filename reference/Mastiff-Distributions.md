# Distribution Classes

Distributions implemented as R6 classes in
[mastiff](https://bdi-pathogens.github.io/mastiff/reference/mastiff-package.md)

## Details

[mastiff](https://bdi-pathogens.github.io/mastiff/reference/mastiff-package.md)
introduces a R6 class structure which is used to combine the density,
distribution function, quantile function and generation of random
deviates into a single object.

Parameters for a distribution are set at initialisation and can be
updated via a named list `$params` stored in the distribution class.

Each distribution includes methods

|         |     |                                                    |
|---------|-----|----------------------------------------------------|
| `$d(x)` |     | Evaluates the density at values `x`                |
| `$p(q)` |     | Evaluates the distribution function at values `q`  |
| `$q(p)` |     | Evaluates the quantile function at values `p`      |
| `$r(n)` |     | Generates `n` random values from the distribution  |
| `$mean` |     | Returns the mean of the distribution               |
| `$sd`   |     | Returns the standard deviation of the distribution |
| `$var`  |     | Returns the variance of the distribution           |

## Discrete Distributions

Discrete distributions with class
[distribution.discrete.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.discrete.class.md)

|                                                                                                                       |                                                                                |
|-----------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| [distribution.binomial](https://bdi-pathogens.github.io/mastiff/reference/distribution.binomial.md)                   | Binomial distribution with size `size` and success probability `prob`          |
| [distribution.negative_binomial](https://bdi-pathogens.github.io/mastiff/reference/distribution.negative_binomial.md) | Negative Binomial distribution with size `size` and success probability `prob` |
| [distribution.point_mass](https://bdi-pathogens.github.io/mastiff/reference/distribution.point_mass.md)               | Point mass at `value`                                                          |
| [distribution.poisson](https://bdi-pathogens.github.io/mastiff/reference/distribution.poisson.md)                     | Poisson distribution with mean `lambda`                                        |

## Continuous Distributions

Discrete distributions with class
[distribution.continuous.class](https://bdi-pathogens.github.io/mastiff/reference/distribution.continuous.class.md)

|                                                                                                           |                                                                  |
|-----------------------------------------------------------------------------------------------------------|------------------------------------------------------------------|
| [distribution.exponential](https://bdi-pathogens.github.io/mastiff/reference/distribution.exponential.md) | Exponential distribution with rate `rate`                        |
| [distribution.gamma](https://bdi-pathogens.github.io/mastiff/reference/distribution.gamma.md)             | Gamma distribution with shape `shape` and rate `rate`            |
| [distribution.normal](https://bdi-pathogens.github.io/mastiff/reference/distribution.normal.md)           | Normal distribution with mean `mean` and standard deviation `sd` |
| [distribution.uniform](https://bdi-pathogens.github.io/mastiff/reference/distribution.uniform.md)         | Uniform distribution on `[min, max]`                             |

## Examples

``` r
# Construct a Poisson( 1 ) random variable
Pois_RV <- distribution.poisson( lambda = 1 )

# Evaluate the density, equivalent to dpois( 0 : 5, lambda = 1 )
Pois_RV$d( 0 : 5 )
#> [1] 0.367879441 0.367879441 0.183939721 0.061313240 0.015328310 0.003065662

# Evaluate the distribution function, equivalent to ppois( 0 : 5, lambda = 1 )
Pois_RV$p( 0 : 5 )
#> [1] 0.3678794 0.7357589 0.9196986 0.9810118 0.9963402 0.9994058

# Evaluate the quantile function, equivalent to qpois( c( 0.5, 0.8 ), lambda = 1 )
Pois_RV$q( c( 0.5, 0.8 ) )
#> [1] 1 2

# Generate random deviates, equivalent to rpois( 10, lambda = 1 )
Pois_RV$r( 10 )
#>  [1] 0 2 1 0 0 1 1 0 1 2

# Update parameters to a Poisson( 10 ) random variable
Pois_RV$params <- list( lambda = 10 )
mean( Pois_RV$r( 1e5 ) )
#> [1] 9.99972
```
