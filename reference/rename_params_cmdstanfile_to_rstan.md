# Renames tensor parameters from cmdstandr to rstan format

In cmdstan output files, tensor parameters are named with their indices
at the end separated by dots, e.g. my_matrix.2.1; in rstan they are
named with their indices at the end internally separated by commas and
then wrapped in square brackets, e.g. my_matrix\[2,1\].

## Usage

``` r
rename_params_cmdstanfile_to_rstan(param_names)
```

## Arguments

- param_names:

  A character vector of param names, before renaming i.e. as found in
  cmdstan output files.

## Value

A character vector of the same length as `param_names`, after renaming.

## Examples

``` r
param_names <- c("foo", "foo.1", "foo.1.2", "foo_1.1.2.3")
rename_params_cmdstanfile_to_rstan(param_names)
#> [1] "foo"          "foo[1]"       "foo[1,2]"     "foo_1[1,2,3]"
```
