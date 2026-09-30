# Run one of rstan, cmdstanr or cmdstan on a file of Stan code

Run one of rstan, cmdstanr or cmdstan on a file of Stan code

## Usage

``` r
run_stan_interfaces(
  path_to_stan_code,
  input_to_stan,
  interface = c("rstan", "cmdstanr", "cmdstan"),
  iter_warmup = 250,
  iter_sampling = 250,
  chains = 4,
  cores = parallel::detectCores(),
  params_to_ignore = character(),
  downsampling_factor = 1L,
  rename_tensor_params = TRUE,
  cmdstan_path_to_installation = NA,
  cmdstan_path_to_output = NA,
  cmdstan_path_to_compiled_model = stringr::str_remove(path_to_stan_code, ".stan$"),
  cmdstan_read_output_into_df = TRUE,
  ...
)
```

## Arguments

- path_to_stan_code:

  the path to the file containing the Stan code.

- input_to_stan:

  a list containing all the input the Stan code expects.

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

- rename_tensor_params:

  a single logical value: if `interface="cmdstan"` or
  `interface="cmdstanr"`, should we run
  [`rename_params_cmdstanfile_to_rstan()`](https://bdi-pathogens.github.io/mastiff/reference/rename_params_cmdstanfile_to_rstan.md)
  on the column names of the resulting dataframe?

- cmdstan_path_to_installation:

  the path to where cmdstan is installed on your system. Inside this
  directory there should be an executable file named `make` (which we
  use to compile Stan code). If `interface="cmdstan"`, the default value
  for this argument is
  [`cmdstanr::cmdstan_path()`](https://mc-stan.org/cmdstanr/reference/set_cmdstan_path.html);
  if not, this argument is not used.

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

## Examples

``` r
  path_to_stan_code <- file.path(system.file("stan", package = "mastiff"), "test.stan")
  writeLines(readLines(path_to_stan_code))
#> data {
#>   int N;
#> }
#> 
#> parameters {
#>   vector<lower = 0, upper = 1>[2] x;
#> }
  run_stan_interfaces(interface = "rstan",
                      path_to_stan_code = path_to_stan_code,
                      input_to_stan = list(N = 1),
                      cores = 1,
                      iter_warmup = 1,
                      iter_sampling = 1)
#> Started running Stan at[1] "2026-09-30 13:03:56 UTC"
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 3e-06 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.03 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: WARNING: No variance estimation is
#> Chain 1:          performed for num_warmup < 20
#> Chain 1: 
#> Chain 1: Iteration: 1 / 2 [ 50%]  (Warmup)
#> Chain 1: Iteration: 2 / 2 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0 seconds (Warm-up)
#> Chain 1:                0 seconds (Sampling)
#> Chain 1:                0 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1e-06 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.01 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: WARNING: No variance estimation is
#> Chain 2:          performed for num_warmup < 20
#> Chain 2: 
#> Chain 2: Iteration: 1 / 2 [ 50%]  (Warmup)
#> Chain 2: Iteration: 2 / 2 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0 seconds (Warm-up)
#> Chain 2:                0 seconds (Sampling)
#> Chain 2:                0 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1e-06 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.01 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: WARNING: No variance estimation is
#> Chain 3:          performed for num_warmup < 20
#> Chain 3: 
#> Chain 3: Iteration: 1 / 2 [ 50%]  (Warmup)
#> Chain 3: Iteration: 2 / 2 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0 seconds (Warm-up)
#> Chain 3:                0 seconds (Sampling)
#> Chain 3:                0 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'anon_model' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1e-06 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.01 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: WARNING: No variance estimation is
#> Chain 4:          performed for num_warmup < 20
#> Chain 4: 
#> Chain 4: Iteration: 1 / 2 [ 50%]  (Warmup)
#> Chain 4: Iteration: 2 / 2 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0 seconds (Warm-up)
#> Chain 4:                0 seconds (Sampling)
#> Chain 4:                0 seconds (Total)
#> Chain 4: 
#> Finished running Stan at[1] "2026-09-30 13:03:56 UTC"
#> Time difference of 0.0758934 secs
#>          x[1]      x[2]      lp__
#>         <num>     <num>     <num>
#> 1: 0.09472183 0.7532501 -4.139062
#> 2: 0.80324466 0.2151966 -3.623416
#> 3: 0.11271253 0.6190417 -3.747149
#> 4: 0.37822915 0.4474447 -2.844843
   # Change the interface. (Here inside \dontrun{} because it needs cmdstan,
   # which is not a given when this code is running in different places.)
   if (FALSE) { # \dontrun{
  run_stan_interfaces(interface = "cmdstanr",
                      path_to_stan_code = path_to_stan_code,
                      input_to_stan = list(N = 1),
                      cores = 1,
                      iter_warmup = 1,
                      iter_sampling = 1)
  run_stan_interfaces(interface = "cmdstan",
                      cmdstan_path_to_output = "temp_",
                      path_to_stan_code = path_to_stan_code,
                      input_to_stan = list(N = 1),
                      cores = 1,
                      iter_warmup = 1,
                      iter_sampling = 1)
   } # }
```
