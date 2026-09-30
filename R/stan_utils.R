#' Run one of rstan, cmdstanr or cmdstan on a file of Stan code
#'
#' @param input_to_stan a list containing all the input the Stan code expects.
#' @param path_to_stan_code the path to the file containing the Stan code.
#' @param interface one of `"rstan"`, `"cmdstanr"` or `"cmdstan"`.
#' @param iter_warmup a positive integer: the number of warmup iterations per
#'   chain (during which the sampling algorithm adapts; these are excluded from
#'   the output).
#' @param iter_sampling a positive integer: the number of sampling iterations
#'   per chain (which are included in the output).
#' @param chains a positive integer: the number of chains used for sampling.
#' @param cores a positive integer: the number cores used in parallel for
#'   computation.
#' @param params_to_ignore a character vector naming parameters to be excluded
#'   from output (if possible; interface dependent).
#' @param downsampling_factor a positive integer: the factor by which to
#'   downsample the posterior. e.g. if a value of 2 is specified, we keep 1 in
#'   every 2 samples. The default of 1 means we keep all samples. TODO:
#'   currently only implemented for cmdstan.
#' @param rename_tensor_params a single logical value: if `interface="cmdstan"`
#'   or `interface="cmdstanr"`, should we run
#'   [rename_params_cmdstanfile_to_rstan()] on the column names of the resulting
#'   dataframe?
#' @param cmdstan_path_to_installation the path to where cmdstan is installed on
#'   your system. Inside this directory there should be an executable file named
#'   `make` (which we use to compile Stan code). If `interface="cmdstan"`, the
#'   default value for this argument is [cmdstanr::cmdstan_path()]; if not, this
#'   argument is not used.
#' @param cmdstan_path_to_output the path to where we will write output files
#'   from cmdstan; you need to specify this if `interface="cmdstan"`, but not
#'   otherwise. Several files will be created with things appended to this path:
#'   _chain1.csv, _chain2.csv etc.
#' @param cmdstan_path_to_compiled_model the path where we will create the
#'   compiled version of the Stan code. Some value (such as the default) is
#'   needed if `interface="cmdstan"`, but not otherwise.
#' @param cmdstan_read_output_into_df a single logical value: should we read
#'   cmdstan output files into a dataframe that is returned by this function? If
#'   a value `FALSE` is specified, this function returns a value `NULL`.
#' @param ... additional arguments will be passed to [rstan::sampling()] (if
#'   `interface="rstan"`) or to the `$sample()` method of the
#'   [cmdstanr::CmdStanModel()] object (if `interface="cmdstanr"`).
#'
#' @returns a dataframe with one row per sample from the posterior and one
#'   column per parameter (unless `interface` is set to `cmdstan` and
#'   `cmdstan_read_output_into_df` is set to `FALSE`).
#' @examples
#'   path_to_stan_code <- file.path(system.file("stan", package = "mastiff"), "test.stan")
#'   writeLines(readLines(path_to_stan_code))
#'   run_stan_interfaces(interface = "rstan",
#'                       path_to_stan_code = path_to_stan_code,
#'                       input_to_stan = list(N = 1),
#'                       cores = 1,
#'                       iter_warmup = 1,
#'                       iter_sampling = 1)
#'    # Change the interface. (Here inside \dontrun{} because it needs cmdstan,
#'    # which is not a given when this code is running in different places.)
#'    \dontrun{
#'   run_stan_interfaces(interface = "cmdstanr",
#'                       path_to_stan_code = path_to_stan_code,
#'                       input_to_stan = list(N = 1),
#'                       cores = 1,
#'                       iter_warmup = 1,
#'                       iter_sampling = 1)
#'   run_stan_interfaces(interface = "cmdstan",
#'                       cmdstan_path_to_output = "temp_",
#'                       path_to_stan_code = path_to_stan_code,
#'                       input_to_stan = list(N = 1),
#'                       cores = 1,
#'                       iter_warmup = 1,
#'                       iter_sampling = 1)
#'    }
#' @export
#'
run_stan_interfaces <- function(path_to_stan_code,
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
                                cmdstan_path_to_compiled_model =
                                  stringr::str_remove(path_to_stan_code, ".stan$"),
                                cmdstan_read_output_into_df = TRUE,
                                ...){

  # Check args
  stopifnot(is.list(input_to_stan))
  stopifnot(is.character(path_to_stan_code))
  stopifnot(length(path_to_stan_code) == 1)
  if (! file.exists(path_to_stan_code)) {
    stop(paste("No file found at", path_to_stan_code))
  }
  stopifnot(endsWith(path_to_stan_code, ".stan"))
  check_numeric(iter_warmup, lower = 1)
  check_numeric(iter_sampling, lower = 1)
  check_numeric(chains, lower = 1)
  check_numeric(cores, lower = 1)
  stopifnot(is.character(params_to_ignore))
  stopifnot(is.character(interface))
  interface <- match.arg(interface)
  if (interface == "cmdstan") {
    if (identical(cmdstan_path_to_output, NA)) stop(paste(
      "If interface is set to cmdstan, the cmdstan_path_to_output option",
      "must be used"
    ))
    if (identical(cmdstan_path_to_installation, NA)) {
      cmdstan_path_to_installation <- cmdstanr::cmdstan_path()
    }
    stopifnot(is.character(cmdstan_path_to_output))
    stopifnot(length(cmdstan_path_to_output) == 1)
    stopifnot(is.character(cmdstan_path_to_installation))
    stopifnot(length(cmdstan_path_to_installation) == 1)
    stopifnot(is.character(cmdstan_path_to_compiled_model))
    stopifnot(length(cmdstan_path_to_compiled_model) == 1)
    stopifnot(dir.exists(cmdstan_path_to_installation))
    cmdstan_path_to_make <- file.path(cmdstan_path_to_installation, "make")
    if (! file.exists(cmdstan_path_to_make)) stop(paste(
      "Could not find a make file inside", cmdstan_path_to_installation))
  }
  check_logical(cmdstan_read_output_into_df)
  check_numeric(downsampling_factor, lower = 1)

  # Compile
  if (interface == "rstan") {
    model_compiled <- rstan::stan_model(path_to_stan_code, auto_write = TRUE)
  } else if (interface == "cmdstanr") {
    model_compiled <- cmdstanr::cmdstan_model(path_to_stan_code)
  } else {
    system(paste("cd", cmdstan_path_to_installation, "&& make STAN_THREADS=true", cmdstan_path_to_compiled_model))
  }

  start_time <- Sys.time()
  cat("Started running Stan at")
  print(start_time)

  if (interface == "rstan") {
    df_samples <- rstan::sampling(model_compiled,
                                  data = input_to_stan,
                                  iter = iter_warmup + iter_sampling,
                                  warmup = iter_warmup,
                                  chains = chains,
                                  cores = cores,
                                  pars = params_to_ignore,
                                  include = FALSE,
                                  ...) %>%
      as.data.frame()
    data.table::setDT(df_samples)

  } else if (interface == "cmdstanr") {
    samples <- model_compiled$sample(
      data = input_to_stan,
      iter_warmup = iter_warmup,
      iter_sampling = iter_sampling,
      chains = chains,
      parallel_chains = cores,
      ...
    )
    df_samples <- samples$draws(format = "draws_df")
    data.table::setDT(df_samples)

  } else {
    files_out_stan <- paste0(cmdstan_path_to_output, "_chain", 1:chains, ".csv")
    files_out_stan_profile <- paste0(cmdstan_path_to_output, "_profiles.csv")
    withr::with_tempfile("cmdstan_path_to_json", fileext = ".json", {
      cmdstanr::write_stan_json(input_to_stan, file = cmdstan_path_to_json)
      command <- paste0(cmdstan_path_to_compiled_model,
                        " method=sample",
                        " num_chains=", chains,
                        " num_warmup=", iter_warmup,
                        " num_samples=", iter_sampling,
                        " num_threads=", cores,
                        " data file=", cmdstan_path_to_json,
                        " output file=", paste(files_out_stan, collapse = ","),
                        " profile_file=", files_out_stan_profile)
      print("About to run this command:")
      print(command)
      system(command)
    })
    if (! all(file.exists(files_out_stan))) stop(paste(
      "Internal error: we expected to create all of the following files, but at",
      "least one does not exist:", paste(files_out_stan, collapse = " ")))
    cat(paste("Created the following output files:",
              paste(files_out_stan, collapse = " "), "\n"))
    if (cmdstan_read_output_into_df) {
      df_samples <- read_cmdstan_out_files(
        file_paths = files_out_stan,
        params_to_ignore = params_to_ignore,
        downsampling_factor = downsampling_factor)
    }
  }

  end_time <- Sys.time()
  cat("Finished running Stan at")
  print(end_time)
  print(end_time - start_time)

  # There is no dataframe to return in this case:
  if (interface == "cmdstan" && ! cmdstan_read_output_into_df) return(NULL)

  # Exclude cols if desired
  keep_col <- rep(TRUE, ncol(df_samples))
  for (param in params_to_ignore) {
    keep_based_on_this_param <-
      colnames(df_samples) != param &
      ! startsWith(colnames(df_samples), paste0(param, ".")) &
      ! startsWith(colnames(df_samples), paste0(param, "["))
    keep_col <- keep_col & keep_based_on_this_param
  }
  df_samples <- df_samples[, ..keep_col]

  if (rename_tensor_params && interface == "cmdstan") {
    data.table::setnames(df_samples, rename_params_cmdstanfile_to_rstan)
  }

  df_samples

}



#' Reads posterior output files from cmdstan into a dataframe
#'
#' @param file_paths the paths of the cmdstan output files. If the file names
#'   end "gz" then we gunzip them (into a pipe, leaving the files themselves
#'   zipped).
#' @param params_to_ignore a character vector naming parameters to be excluded
#'   from output. If you name a tensor parameter (e.g. `"my_vector"`), we exclude
#'   all elements of it (e.g. `"my_vector.1"`, `"my_vector.2"`).
#' @param downsampling_factor a positive integer: the factor by which to
#'   downsample the posterior. e.g. if a value of 2 is specified, we keep 1 in
#'   every 2 samples. The default of 1 means we keep all samples.
#' @param comment_lines a single logical value: do the cmdstan output files
#'   contain comment lines (beginning with #)? Normally they do; set this
#'   argument to FALSE only if you have already removed such lines (for slightly
#'   faster parsing of the files).
#' @param verbose a single logical value: should we update on progress reading
#'   in the files?
#'
#' @returns a dataframe binding the contents of all the output files: one row
#'   per sample from the posterior, one column per parameter (and an extra
#'   column indicating which chain that sample came from, if that can be
#'   detected from the file names).
#' @export
#'
read_cmdstan_out_files <- function(file_paths,
                                   params_to_ignore = character(),
                                   downsampling_factor = 1L,
                                   comment_lines = TRUE,
                                   verbose = TRUE) {

  stopifnot(is.character(file_paths))
  stopifnot(length(file_paths) > 0)
  stopifnot(all(file.exists(file_paths)))
  stopifnot(is.character(params_to_ignore))
  check_numeric(downsampling_factor, lower = 1, upper_inclusive = FALSE)

  purrr::map(file_paths, function(file_){
    if (verbose) {
      print(Sys.time())
      cat("Now reading file", file_, "\n")
    }
    if (comment_lines) {
      if (endsWith(file_, "gz")) {
        cmd <- paste("gunzip -c", file_, "| grep -v '^#'")
      } else {
        cmd <- paste("grep -v '^#'", file_)
      }
      df_ <- data.table::fread(cmd = cmd)
    } else {
      df_ <- data.table::fread(file_)
    }
    if (length(params_to_ignore)) {
      keep_col <- rep(TRUE, ncol(df_))
      for (param in params_to_ignore) {
        keep_based_on_this_param <-
          colnames(df_) != param &
          ! startsWith(colnames(df_), paste0(param, "."))
        keep_col <- keep_col & keep_based_on_this_param
      }
      df_ <- df_[, ..keep_col]
      if (verbose) {
        cat("Reduced from", length(keep_col), "to", ncol(df_),
            "columns due to params_to_ignore\n")
      }
    }
    if (downsampling_factor > 1L) {
      df_ <- df_[seq(1, .N, by = downsampling_factor)]
    }
    chain <- stringr::str_match(file_, "chain_([0-9+])")[,2]
    if (!is.na(chain)) {
      df_[, chain := as.integer(chain)]
    }
    df_
  }) %>% data.table::rbindlist(use.names = TRUE)

}

#' Renames tensor parameters from cmdstan to rstan format
#'
#' In cmdstan output files, tensor parameters are named with their indices at the
#' end separated by dots, e.g. my_matrix.2.1; in rstan they are named with their
#' indices at the end internally separated by commas and then wrapped in square
#' brackets, e.g. my_matrix\[2,1\].
#'
#' @param param_names A character vector of param names, before renaming i.e. as
#'   found in cmdstan output files.
#'
#' @returns A character vector of the same length as `param_names`, after
#'   renaming.
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_match
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' param_names <- c("foo", "foo.1", "foo.1.2", "foo_1.1.2.3")
#' rename_params_cmdstanfile_to_rstan(param_names)
rename_params_cmdstanfile_to_rstan <- function(param_names) {
  stopifnot(is.character(param_names))
  map_chr(param_names, function(name) {
    tensor_suffix <- stringr::str_match(name, "\\.([.0-9]+)$")[,2]
    if (is.na(tensor_suffix)) return(name)
    tensor_suffix_length <- nchar(tensor_suffix)
    piece_before_suffix <- substr(name, 1, nchar(name) - tensor_suffix_length - 1)
    tensor_suffix <- stringr::str_replace_all(tensor_suffix, "\\.", ",")
    paste0(piece_before_suffix, "[", tensor_suffix, "]")
  })
}

#' Get a regex for any warnings that Stan may return due to too few iterations,
#' which we want to ignore e.g. during testing.
#'
#' @returns a length-1 character vector
#' @noRd
stan_safe_warnings <- function() paste0(
  "Bulk Effective Samples Size \\(ESS\\) is too low",
  "|Tail Effective Samples Size \\(ESS\\) is too low",
  "|indicating chains have not mixed.",
  "|E-BFMI not computed because it is undefined for posterior chains of length less than",
  "|divergent transitions after warmup",
  "|Examine the pairs\\(\\) plot to diagnose sampling problems",
  "|transitions after warmup that exceeded the maximum treedepth",
  "|where the estimated Bayesian Fraction of Missing Information was low"
)
