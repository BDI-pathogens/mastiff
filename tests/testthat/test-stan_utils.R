# We only do local testing of run_stan_interfaces(interface = "cmdstan")
# so that cmdstan does not need to be installed every time we test on github
testing_locally <- FALSE

# TEST read_cmdstan_out_files ----

test_that("read_cmdstan_out_files works for 1 file", {
  withr::with_tempfile("tempfile_", {
    cat("x.1,x.2", file = tempfile_)
    df_ <- read_cmdstan_out_files(tempfile_)
    expect_all_true(colnames(df_) == c("x.1", "x.2"))
    expect_equal(nrow(df_), 0)
  })
})

test_that("read_cmdstan_out_files works for 2 files", {
  withr::with_tempdir({
    path <- getwd()
    tempfile1 <- paste0(path, "/temp1")
    tempfile2 <- paste0(path, "/temp2")
    cat("x", file = tempfile1)
    cat("x", file = tempfile2)
    df_ <- read_cmdstan_out_files(c(tempfile1, tempfile2))
    expect_true(colnames(df_) == "x")
    expect_equal(nrow(df_), 0)
  })
})

test_that("read_cmdstan_out_files works with params_to_ignore", {
  withr::with_tempfile("tempfile_", {
    cat("x.1,x.2", file = tempfile_)
    df_ <- read_cmdstan_out_files(tempfile_, params_to_ignore = "x")
    expect_equal(ncol(df_), 0)
  })
})

test_that("read_cmdstan_out_files works for gzipped file", {
  withr::with_tempfile("tempfile_", fileext = ".gz", {
    tempfile_stream <- gzfile(tempfile_, "w")
    cat("x.1,x.2", file = tempfile_stream)
    close(tempfile_stream)
    #system(paste("gunzip -c", tempfile_)) # check it really is a gzipped file
    df_ <- read_cmdstan_out_files(tempfile_)
    expect_all_true(colnames(df_) == c("x.1", "x.2"))
    expect_equal(nrow(df_), 0)
  })
})

test_that("read_cmdstan_out_files works for gzipped file with comments", {
  tempfile_ <- "temp.gz"
  withr::with_tempfile("tempfile_", fileext = ".gz", {
    tempfile_stream <- gzfile(tempfile_, "w")
    cat(c("x.1,x.2", "# comment", "1,2"), file = tempfile_stream, sep = "\n")
    close(tempfile_stream)
    #system(paste("gunzip -c", tempfile_)) # check it really is a gzipped file
    df_ <- read_cmdstan_out_files(tempfile_)
    expect_all_true(colnames(df_) == c("x.1", "x.2"))
    expect_equal(nrow(df_), 1)
  })
})

test_that("read_cmdstan_out_files works with comment_lines == FALSE and no comments", {
  withr::with_tempfile("tempfile_", {
    cat("x.1,x.2", file = tempfile_)
    df_ <- read_cmdstan_out_files(tempfile_, comment_lines = FALSE)
    expect_all_true(colnames(df_) == c("x.1", "x.2"))
    expect_equal(nrow(df_), 0)
  })
})

test_that("read_cmdstan_out_files breaks with comment_lines == FALSE and with comments", {
  withr::with_tempfile("tempfile_", {
    cat(c("x.1,x.2", "# comment", "1,2"), file = tempfile_, sep = "\n")
    expect_warning(read_cmdstan_out_files(tempfile_, comment_lines = FALSE),
                   regexp = "Detected 1 column names but the data has 2 columns")
  })
})

test_that("read_cmdstan_out_files breaks for gzipped file with comment_lines == FALSE and with comments", {
  tempfile_ <- "temp.gz"
  withr::with_tempfile("tempfile_", {
    tempfile_stream <- gzfile(tempfile_, "w")
    cat(c("x.1,x.2", "# comment", "1,2"), file = tempfile_stream, sep = "\n")
    close(tempfile_stream)
    #system(paste("gunzip -c", tempfile_)) # check it really is a gzipped file
    expect_warning(read_cmdstan_out_files(tempfile_, comment_lines = FALSE),
                   regexp = "Detected 1 column names but the data has 2 columns")
  })
})

# TEST run_stan_interfaces ----

if (testing_locally) {

test_that("run_stan_interfaces works with cmdstan", {
  withr::with_tempdir({
    path <- getwd()
    cmdstan_output_basename <- paste0(path, "/temp_dvsb_out")
    df_posterior <- run_stan_interfaces(
      input_to_stan = list(N=1),
      path_to_stan_code = get_stan_test_file_path(),
      interface = "cmdstan",
      iter_warmup = 1,
      iter_sampling = 1,
      cmdstan_path_to_output = cmdstan_output_basename)
  })
  expect_true(is.data.frame(df_posterior))
  expect_true(all(c("x[1]", "x[2]") %in% colnames(df_posterior)))
})

test_that("run_stan_interfaces works with cmdstan without renaming option", {
  withr::with_tempdir({
    path <- getwd()
    cmdstan_output_basename <- paste0(path, "/temp_dvsb_out")
    df_posterior <- run_stan_interfaces(
      input_to_stan = list(N=1),
      path_to_stan_code = get_stan_test_file_path(),
      interface = "cmdstan",
      rename_tensor_params = FALSE,
      iter_warmup = 1,
      iter_sampling = 1,
      cmdstan_path_to_output = cmdstan_output_basename)
  })
  expect_true(is.data.frame(df_posterior))
  expect_true(all(c("x.1", "x.2") %in% colnames(df_posterior)))
})

test_that("run_stan_interfaces with cmdstan returns NULL if cmdstan_read_output_into_df == FALSE", {
  withr::with_tempdir({
    path <- getwd()
    cmdstan_output_basename <- paste0(path, "/temp_dvsb_out")
    result <- run_stan_interfaces(
      input_to_stan = list(N=1),
      path_to_stan_code = get_stan_test_file_path(),
      interface = "cmdstan",
      iter_warmup = 1,
      iter_sampling = 1,
      cmdstan_path_to_output = cmdstan_output_basename,
      cmdstan_read_output_into_df = FALSE)
  })
  expect_true(is.null(result))
})

test_that("run_stan_interfaces works with cmdstanr", {
  df_posterior <- pkgcond::suppress_warnings(run_stan_interfaces(
    input_to_stan = list(N=1),
    path_to_stan_code = get_stan_test_file_path(),
    interface = "cmdstanr",
    cores = 1,
    iter_warmup = 1,
    iter_sampling = 1),
    stan_safe_warnings())
  expect_true(is.data.frame(df_posterior))
  expect_true(all(c("x[1]", "x[2]") %in% colnames(df_posterior)))
})

}

test_that("run_stan_interfaces works with rstan", {
  df_posterior <- pkgcond::suppress_warnings(run_stan_interfaces(
    input_to_stan = list(N=1),
    path_to_stan_code = get_stan_test_file_path(),
    interface = "rstan",
    cores = 1,
    iter_warmup = 1,
    iter_sampling = 1),
    stan_safe_warnings())
  expect_true(is.data.frame(df_posterior))
  expect_true(all(c("x[1]", "x[2]") %in% colnames(df_posterior)))
})
