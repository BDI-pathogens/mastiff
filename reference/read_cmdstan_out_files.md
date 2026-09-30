# Reads posterior output files from cmdstan into a dataframe

Reads posterior output files from cmdstan into a dataframe

## Usage

``` r
read_cmdstan_out_files(
  file_paths,
  params_to_ignore = character(),
  downsampling_factor = 1L,
  comment_lines = TRUE,
  verbose = TRUE
)
```

## Arguments

- file_paths:

  the paths of the cmdstan output files. If the file names end "gz" then
  we gunzip them (into a pipe, leaving the files themselves zipped).

- params_to_ignore:

  a character vector naming parameters to be excluded from output. If
  you name a tensor parameter (e.g. `"my_vector"`), we exclude all
  elements of it (e.g. `"my_vector.1"`, `"my_vector.2"`).

- downsampling_factor:

  a positive integer: the factor by which to downsample the posterior.
  e.g. if a value of 2 is specified, we keep 1 in every 2 samples. The
  default of 1 means we keep all samples.

- comment_lines:

  a single logical value: do the cmdstan output files contain comment
  lines (beginning with \#)? Normally they do; set this argument to
  FALSE only if you have already removed such lines (for slightly faster
  parsing of the files).

- verbose:

  a single logical value: should we update on progress reading in the
  files?

## Value

a dataframe binding the contents of all the output files: one row per
sample from the posterior, one column per parameter (and an extra column
indicating which chain that sample came from, if that can be detected
from the file names).
