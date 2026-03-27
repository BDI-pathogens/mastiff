# Vectorised uniroot

A vectorised version of the Brent root algorithm. Useful for solving
many similar optimisation problems where evaluation of the objective
function can be calculated more efficiently when vectorised

## Usage

``` r
uniroot.vectorized(f, lower, upper, tol = 1e-08, itmax = 100, eps = 1e-10)
```

## Arguments

- f:

  the objective function to optimise over. Must take a single vector as
  input and output a vector of values

- lower:

  vector of left hand end points of the intervals to optimise over

- upper:

  vector of right hand end points of the intervals to optimise over

- tol:

  the desired accuracy (convergence tolerance)

- itmax:

  the maximum number of iterations

- eps:

  XXXXX TODO

## Value

vector of roots for each interval `[lower, upper ]`

## Examples

``` r
f <- function( x ) ( x^2 - 1 ) * ( x^2 - 2 )
lower <- c( -2,  -1.2,   0, 1.2 )
upper <- c( -1.2,   0, 1.2,   2 )
uniroot.vectorized( f, lower, upper )
#> [1] -1.414214 -1.000000  1.000000  1.414214
```
