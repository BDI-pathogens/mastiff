# Checks a variable is a single number (and optionally, in a range and not NA)

Checks a variable is a single number (and optionally, in a range and not
NA)

## Usage

``` r
check_numeric(
  x,
  lower = -Inf,
  lower_inclusive = TRUE,
  upper = Inf,
  upper_inclusive = TRUE,
  allow_missing = FALSE
)
```

## Arguments

- x:

  The variable to check.

- lower:

  The lower limit for `x` (default = `-Inf`).

- lower_inclusive:

  Whether the lower limit itself is inclusive, i.e. whether we require
  `x >= lower` or `x > lower` (the default is the former).

- upper:

  The upper limit for `x` (default = `Inf`).

- upper_inclusive:

  Whether the upper limit itself is inclusive, i.e. whether we require
  `x <= upper` or `x < upper` (the default is the former).

- allow_missing:

  Whether x is allowed to be missing (default = `FALSE`). If
  `allow_missing` is `TRUE` we skip the tests comparing `x` to `lower`
  and `upper`.

## Value

`invisible(TRUE)`, if no problems were found; otherwise a
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) is triggered.

## Examples

``` r
# These are all OK:
check_numeric(0)
check_numeric(0L)
check_numeric(0, lower = 0)
check_numeric(0, upper = 0)
check_numeric(NA_real_, allow_missing = TRUE)
check_numeric(Inf)
check_numeric(-Inf)
# These return errors:
try(check_numeric("foo"))
#> Error in check_numeric("foo") : is.numeric(x) is not TRUE
try(check_numeric(1:2))
#> Error in check_numeric(1:2) : length(x) == 1 is not TRUE
try(check_numeric(NA_real_))
#> Error in check_numeric(NA_real_) : !is.na(x) is not TRUE
try(check_numeric(NA, allow_missing = TRUE)) # NA is logical, not numeric
#> Error in check_numeric(NA, allow_missing = TRUE) : 
#>   is.numeric(x) is not TRUE
try(check_numeric(0, lower = 1))
#> Error in check_numeric(0, lower = 1) : x >= lower is not TRUE
try(check_numeric(0, lower = 0, lower_inclusive = FALSE))
#> Error in check_numeric(0, lower = 0, lower_inclusive = FALSE) : 
#>   x > lower is not TRUE
```
