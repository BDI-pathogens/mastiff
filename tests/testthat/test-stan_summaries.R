
test_that("Good args to stanfit_to_matrix and stanfit_to_dt work", {
  stanfit_ <- stan_example_regression$posterior_samples
  expect_true(is.matrix(stanfit_to_matrix(stanfit_)))
  expect_true(data.table::is.data.table(stanfit_to_dt(stanfit_)))
  expect_equal(ncol(stanfit_to_matrix(stanfit_, params_desired = c("c", "m"))), 2)
  expect_equal(ncol(stanfit_to_dt(stanfit_, params_desired = c("c", "m"))), 2)
})

test_that("Bad args to stanfit_to_matrix give appropriate errors or warnings", {
  expect_error(stanfit_to_matrix("foo"), fixed = TRUE,
               regex = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(stanfit_to_matrix(stanfit_, params_desired = 1), fixed = TRUE,
               regex = 'is.character(params_desired) is not TRUE')
  expect_error(stanfit_to_matrix(stanfit_, params_desired = character()), fixed = TRUE,
               regex = 'length(params_desired) > 0L is not TRUE')
  expect_warning(stanfit_to_matrix(stanfit_, params_desired = c("m", "m")),
                 fixed = TRUE, regexp = "Duplicates are present in params_desired. Ignoring them.")
  expect_error(stanfit_to_matrix(stanfit_, params_desired = "foo"),
               fixed = TRUE, regexp = "Parameter foo not present in stanfit object")
})

test_that("Bad args to stanfit_to_dt give appropriate errors or warnings", {
  expect_error(stanfit_to_dt("foo"), fixed = TRUE,
               regex = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(stanfit_to_dt(stanfit_, params_desired = 1), fixed = TRUE,
               regex = 'is.character(params_desired) is not TRUE')
  expect_error(stanfit_to_dt(stanfit_, params_desired = character()), fixed = TRUE,
               regex = 'length(params_desired) > 0L is not TRUE')
  expect_warning(stanfit_to_dt(stanfit_, params_desired = c("m", "m")),
                 fixed = TRUE, regexp = "Duplicates are present in params_desired. Ignoring them.")
  expect_error(stanfit_to_dt(stanfit_, params_desired = "foo"),
               fixed = TRUE, regexp = "Parameter foo not present in stanfit object")
})


test_that("summaries of stan_example_regression data = snapshotted values", {

  # Note if the stan_example_regression data is regenerated for some reason,
  # new numerical values will be needed.

  # means
  result_expected <- array(c(3.008395136, 2.080654432, 1.007372842,
                             3.002214112, 1.982219439, 0.994800462),
                           dim = c(3, 2),
                           dimnames = list(c("m", "c", "sigma"), NULL))
  stanfit_list <- list(stan_example_regression$posterior_samples,
                       stan_example_regression$prior_samples)
  expect_equal(posterior_means(stanfit_list), result_expected)

  # medians
  result_expected <- array(c(3.008573743, 2.081750608, 0.983070997,
                             3.019150968, 1.931861168, 0.984441233),
                           dim = c(3, 2),
                           dimnames = list(c("m", "c", "sigma"), NULL))
  stanfit_list <- list(stan_example_regression$posterior_samples,
                       stan_example_regression$prior_samples)
  expect_equal(posterior_medians(stanfit_list), result_expected)

  # 95% central intervals
  results_expected <- array(c(
    2.932195551, 1.143433866, 0.724979458, 3.084792794, 2.989257058, 1.425863744,
    0.183432935, 0.090838915, 0.053826474, 5.820426929, 3.891101640, 1.959116536),
    dim = c(3, 2, 2), dimnames = list(c("m", "c", "sigma"), c("2.5%", "97.5%"), NULL))
  expect_equal(posterior_intervals(stanfit_list, 0.95), results_expected)

  # mass in a range
  results_expected <- 0.41375
  expect_equal(posterior_mass_in_range(stan_example_regression$posterior_samples,
                                       param = "m", range = c(-Inf, 3)),
               results_expected)
})

test_that("Using params_desired restricts output to expected dimensions", {
  expect_equal(dim(posterior_means(stanfit_list, params_desired = c("c", "m"))),
               c(2, 2))
  expect_equal(dim(posterior_medians(stanfit_list, params_desired = c("c", "m"))),
               c(2, 2))
  expect_equal(dim(posterior_intervals(stanfit_list, params_desired = c("c", "m"), prob = 0.95)),
               c(2, 2, 2))
})

test_that("Bad args give appropriate errors", {
  expect_error(posterior_means("foo"), fixed = TRUE,
               regex = "is.list(stanfit_list) is not TRUE")
  expect_error(posterior_means(list("foo")), fixed = TRUE,
               regex = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(posterior_medians("foo"), fixed = TRUE,
               regex = "is.list(stanfit_list) is not TRUE")
  expect_error(posterior_medians(list("foo")), fixed = TRUE,
               regex = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(posterior_intervals("foo", prob = 0.95), fixed = TRUE,
               regex = "is.list(stanfit_list) is not TRUE")
  expect_error(posterior_intervals(list("foo"), prob = 0.95), fixed = TRUE,
               regex = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(posterior_intervals(stanfit_list, prob = "foo"), fixed = TRUE,
               regex = 'is.numeric(x) is not TRUE')
  expect_error(posterior_intervals(stanfit_list, prob = -1), fixed = TRUE,
               regex = 'lower <= x is not TRUE')
  expect_error(posterior_intervals(stanfit_list, prob = 2), fixed = TRUE,
               regex = 'x <= upper is not TRUE')
  expect_error(posterior_intervals(stanfit_list, prob = NA_real_), fixed = TRUE,
               regex = '!is.na(x) is not TRUE')
  expect_error(posterior_intervals(stanfit_list, prob = 0:1), fixed = TRUE,
               regex = 'length(x) == 1 is not TRUE')
  expect_error(posterior_mass_in_range("foo", param = "m", range = 0:1),
               fixed = TRUE, regex = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(posterior_mass_in_range(stan_example_regression$posterior_samples,
                                       param = 1, range = 0:1),
               fixed = TRUE, regex = 'is.character(param) is not TRUE')
  expect_error(posterior_mass_in_range(stan_example_regression$posterior_samples,
                                       param = c("c", "m"), range = 0:1),
               fixed = TRUE, regex = 'length(param) == 1 is not TRUE')
  expect_error(posterior_mass_in_range(stan_example_regression$posterior_samples,
                                       param = "m", range = "foo"),
               fixed = TRUE, regex = 'is.numeric(range) is not TRUE')
  expect_error(posterior_mass_in_range(stan_example_regression$posterior_samples,
                                       param = "m", range = 1),
               fixed = TRUE, regex = 'length(range) == 2 is not TRUE')
  expect_error(posterior_mass_in_range(stan_example_regression$posterior_samples,
                                       param = "m", range = c(1, 0)),
               fixed = TRUE, regex = 'range[[1]] <= range[[2]] is not TRUE')
})
