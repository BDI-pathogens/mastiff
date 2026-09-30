# Run one of rstan, cmdstanr or cmdstan on a file of Stan code

Run one of rstan, cmdstanr or cmdstan on a file of Stan code

## Usage

``` r
run_stan_interfaces(
  input_to_stan,
  path_to_stan_code,
  interface = c("rstan", "cmdstanr", "cmdstan"),
  iter_warmup = 250,
  iter_sampling = 250,
  chains = 4,
  cores = parallel::detectCores(),
  params_to_ignore = character(),
  downsampling_factor = 1L,
  cmdstan_path_to_installation = NA,
  cmdstan_path_to_json = NA,
  cmdstan_overwrite_json = FALSE,
  cmdstan_path_to_output = NA,
  cmdstan_path_to_compiled_model = stringr::str_remove(path_to_stan_code, ".stan$"),
  cmdstan_read_output_into_df = TRUE,
  ...
)
```

## Arguments

- input_to_stan:

  a list containing all the input the Stan code expects.

- path_to_stan_code:

  the path to the file containing the Stan code.

- interface:

  one of `"rstan"`, `"cmdstanr"` or `"cmdstan"`.

- iter_warmup:

  a positive integer: the number of warmup iterations per chain (during
  which the sampling algorithm adapts; these are excluded from the
  output).

- iter_sampling:

  a positive integer: the number of sampling iterations per chain (which
  are included in the output).

- chains:

  a positive integer: the number of chains used for sampling.

- cores:

  a positive integer: the number cores used in parallel for computation.

- params_to_ignore:

  a character vector naming parameters to be excluded from output (if
  possible; interface dependent).

- downsampling_factor:

  a positive integer: the factor by which to downsample the posterior.
  e.g. if a value of 2 is specified, we keep 1 in every 2 samples. The
  default of 1 means we keep all samples. TODO: currently only
  implemented for cmdstan.

- cmdstan_path_to_installation:

  the path to where cmdstan is installed on your system; you need to
  specify this if `interface="cmdstan"`, but not otherwise. Inside this
  directory there should be an executable file named `make` (which we
  use to compile Stan code).

- cmdstan_path_to_json:

  the path to where we will write a temporary json file to hold the
  input for cmdstan; you need to specify this if `interface="cmdstan"`,
  but not otherwise.

- cmdstan_overwrite_json:

  a single logical value: should we overwrite a file at
  `cmdstan_path_to_json` if it exists already?

- cmdstan_path_to_output:

  the path to where we will write output files from cmdstan; you need to
  specify this if `interface="cmdstan"`, but not otherwise. Several
  files will be created with things appended to this path: \_chain1.csv,
  \_chain2.csv etc.

- cmdstan_path_to_compiled_model:

  the path where we will create the compiled version of the Stan code.
  Some value (such as the default) is needed if `interface="cmdstan"`,
  but not otherwise.

- cmdstan_read_output_into_df:

  a single logical value: should we read cmdstan output files into a
  dataframe that is returned by this function? If a value `FALSE` is
  specified, this function returns a value `NULL`.

- ...:

  additional arguments will be passed to
  [`rstan::sampling()`](https://mc-stan.org/rstan/reference/stanmodel-method-sampling.html)
  (if `interface="rstan"`) or to the `$sample()` method of the
  [`cmdstanr::CmdStanModel()`](https://mc-stan.org/cmdstanr/reference/CmdStanModel.html)
  object (if `interface="cmdstanr"`).

## Value

a dataframe with one row per sample from the posterior and one column
per parameter (unless `interface` is set to `cmdstan` and
`cmdstan_read_output_into_df` is set to `FALSE`).
