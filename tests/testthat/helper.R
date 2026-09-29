get_stan_test_file_path <- function() {
  stan_subdir <- system.file("stan", package = "mastiff")
  stan_path <- file.path(stan_subdir, "test.stan")
  if (! file.exists(stan_path)) {
    stop(paste0("test.stan not found inside the expected directory of ",
                stan_path, " , which contains only: ",
                paste(list.files(stan_subdir), collapse = " ")))
  }
  stan_path
}
