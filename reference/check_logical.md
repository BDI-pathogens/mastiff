# Checks a variable is a single logical value (and optionally not missing)

Checks a variable is a single logical value (and optionally not missing)

## Usage

``` r
check_logical(x, allow_missing = FALSE)
```

## Arguments

- x:

  The variable to check.

- allow_missing:

  Whether x is allowed to be missing (default = `FALSE`).

## Value

`invisible(TRUE)`, if no problems were found; otherwise a
[`stopifnot()`](https://rdrr.io/r/base/stopifnot.html) is triggered.

## Examples

``` r
# These are all OK:
check_logical(TRUE)
check_logical(FALSE)
check_logical(NA, allow_missing = TRUE)
# These return errors:
try(check_logical("foo"))
#> Error in check_logical("foo") : is.logical(x) is not TRUE
try(check_logical(c(TRUE, TRUE)))
#> Error in check_logical(c(TRUE, TRUE)) : length(x) == 1 is not TRUE
try(check_logical(NA))
#> Error in check_logical(NA) : !is.na(x) is not TRUE
try(check_logical(NA_real_, allow_missing = TRUE)) # NA_real_ is not logical
#> Error in check_logical(NA_real_, allow_missing = TRUE) : 
#>   is.logical(x) is not TRUE
```
