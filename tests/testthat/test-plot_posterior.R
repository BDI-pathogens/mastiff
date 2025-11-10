# Test each of the 2^5
# possibilities for the 5 optional arguments (i.e. specifying them or not).
# (Transforming a param without renaming it makes no sense, nor does renaming
# it without transforming it (as below at least), but just to test the idea.)
test_that("prior & posterior stanfit arguments work for 2^5 arg possibilities", {
  posterior <- stan_example_regression$posterior_samples
  prior     <- stan_example_regression$prior_samples
  true_vals <- stan_example_regression$true_values
  expect_no_error(plot_posterior(posterior))
  expect_no_error(plot_posterior(posterior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, prior_samples = prior))
  expect_no_error(plot_posterior(posterior, prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma")))
  expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log)))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma")))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)")))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma")))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log)))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma")))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior))
  expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals))
})

test_that("prior & posterior data.table arguments work for 2^5 arg possibilities", {
  posterior_dt <- data.table::data.table(m = stats::rnorm(100),
                                         c = stats::rnorm(100, mean = 10),
                                         sigma = stats::rlnorm(100))
  prior_dt <- data.table::data.table(m = stats::rnorm(100, sd = 2),
                                     c = stats::rnorm(100, mean = 10, sd = 2),
                                     sigma = stats::rlnorm(100, sdlog = 2))
  true_vals_dt <- c(m = 0, c = 10, sigma = 1)
  expect_no_error(plot_posterior(posterior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE))
  expect_no_error(plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE))
})


# Test
test_that("bad inputs give appropriate errors or warnings", {

  posterior <- stan_example_regression$posterior_samples
  prior     <- stan_example_regression$prior_samples
  true_vals <- stan_example_regression$true_values
  posterior_dt <- data.table::data.table(m = stats::rnorm(100),
                                         c = stats::rnorm(100, mean = 10),
                                         sigma = stats::rlnorm(100))
  prior_dt <- data.table::data.table(m = stats::rnorm(100, sd = 2),
                                     c = stats::rnorm(100, mean = 10, sd = 2),
                                     sigma = stats::rlnorm(100, sdlog = 2))
  true_vals_dt <- c(m = 0, c = 10, sigma = 1)
  prior_dt_bad <- prior_dt
  prior_dt_bad$m <- NULL

  expect_error(plot_posterior(posterior, skip_stanfit_to_dt = "foo"),
               fixed = TRUE, regexp = "is.logical(skip_stanfit_to_dt) is not TRUE")
  expect_error(plot_posterior("foo"),
               fixed = TRUE, regexp = 'class(stanfit)[[1]] == "stanfit" is not TRUE')
  expect_error(plot_posterior("foo", skip_stanfit_to_dt = TRUE),
               fixed = TRUE, regexp = "is.data.frame(posterior_samples) is not TRUE")
  expect_error(plot_posterior(posterior, params_desired = 1),
               fixed = TRUE, regexp = "is.character(params_desired) is not TRUE")
  expect_error(plot_posterior(posterior_dt, params_desired = 1, skip_stanfit_to_dt = TRUE),
               fixed = TRUE, regexp = "is.character(params_desired) is not TRUE")
  expect_error(plot_posterior(posterior, params_desired = character()),
               fixed = TRUE, regexp = "length(params_desired) > 0L is not TRUE")
  expect_error(plot_posterior(posterior_dt, skip_stanfit_to_dt = TRUE, params_desired = character()),
               fixed = TRUE, regexp = "length(params_desired) > 0L is not TRUE")
  expect_warning(plot_posterior(posterior, params_desired = c("m", "m")),
                 fixed = TRUE, regexp = "Duplicates are present in params_desired. Ignoring them.")
  expect_warning(plot_posterior(posterior_dt, skip_stanfit_to_dt = TRUE, params_desired = c("m", "m")),
                 fixed = TRUE, regexp = "Duplicates are present in params_desired. Ignoring them.")
  expect_error(plot_posterior(posterior, params_desired = "foo"),
               fixed = TRUE, regexp = "Parameter foo not present in stanfit object")
  expect_error(plot_posterior(posterior_dt, skip_stanfit_to_dt = TRUE, params_desired = "foo"),
               fixed = TRUE, regexp = "Parameter foo not present in posterior_samples")
  expect_error(plot_posterior(posterior_dt, prior_samples = "foo", skip_stanfit_to_dt = TRUE),
               fixed = TRUE, regexp = "is.data.frame(prior_samples) is not TRUE")
  expect_error(plot_posterior(posterior_dt, prior_samples = prior_dt_bad, skip_stanfit_to_dt = TRUE, params_desired = "m"),
               fixed = TRUE, regexp = "Parameter m not present in prior_samples")
  expect_error(plot_posterior(posterior_dt, prior_samples = prior_dt_bad, skip_stanfit_to_dt = TRUE),
               fixed = TRUE, regexp = "identical(sort(names(dt_posterior)), sort(names(dt_prior))) is not TRUE")
  expect_error(plot_posterior(posterior, true_param_values = "foo"),
               fixed = TRUE, regexp = "is.numeric(true_param_values) is not TRUE")
  expect_error(plot_posterior(posterior, true_param_values = numeric()),
               fixed = TRUE, regexp = "!is.null(names(true_param_values)) is not TRUE")
  expect_error(plot_posterior(posterior, true_param_values = 1:3),
               fixed = TRUE, regexp = "!is.null(names(true_param_values)) is not TRUE")
  expect_error(plot_posterior(posterior, true_param_values = c(sigma = 1, sigma = 2)),
               fixed = TRUE, regexp = "!anyDuplicated(names(true_param_values)) is not TRUE")
  expect_warning(plot_posterior(posterior, true_param_values = c(foo = 1)),
                 fixed = TRUE, regexp = paste(
                   "Ignoring the following params which had true values specified,",
                   "but were not found in the posterior samples: foo"))
  expect_error(plot_posterior(posterior, transforms = "foo"),
               fixed = TRUE, regexp = "is.list(transforms) is not TRUE")
  expect_error(plot_posterior(posterior, transforms = list(log)),
               fixed = TRUE, regexp = "!is.null(names(transforms)) is not TRUE")
  expect_error(plot_posterior(posterior, transforms = list(sigma = log, sigma = log)),
               fixed = TRUE, regexp = "!anyDuplicated(names(transforms)) is not TRUE")
  expect_error(plot_posterior(posterior, transforms = list(foo="foo")),
               fixed = TRUE, regexp = "At least one element in the transforms list is not a function")
  expect_error(plot_posterior(posterior, transforms = list(foo=log)),
               fixed = TRUE, regexp = "all(names(transforms) %in% params) is not TRUE")
  expect_error(plot_posterior(posterior, labels = 1),
               fixed = TRUE, regexp = "is.character(labels) is not TRUE")
  expect_error(plot_posterior(posterior, labels = c("log(sigma)")),
               fixed = TRUE, regexp = "!is.null(names(labels)) is not TRUE")
  expect_error(plot_posterior(posterior, labels = c(sigma = "foo", sigma = "bar")),
               fixed = TRUE, regexp = "!anyDuplicated(names(labels)) is not TRUE")
  expect_error(plot_posterior(posterior, labels = c(foo = "foo")),
               fixed = TRUE, regexp = "all(names(labels) %in% params) is not TRUE")
  expect_error(plot_posterior(posterior, bins = "foo"),
               fixed = TRUE, regexp = "is.numeric(x) is not TRUE")
  expect_error(plot_posterior(posterior, bins = 0),
               fixed = TRUE, regexp = "x >= lower is not TRUE")
  expect_error(plot_posterior(posterior, lower = "foo"),
               fixed = TRUE, regexp = "is.numeric(lower) is not TRUE")
  expect_error(plot_posterior(posterior, lower = numeric()),
               fixed = TRUE, regexp = "length(lower) > 0 is not TRUE")
  expect_error(plot_posterior(posterior, lower = 1:2),
               fixed = TRUE,
               regexp = "If the 'lower' arg has length > 1, it must be a named numeric vector")
  expect_error(plot_posterior(posterior, lower = c(a=1, a=2)),
               fixed = TRUE, regexp = "!anyDuplicated(names(lower)) is not TRUE")
  expect_error(plot_posterior(posterior, lower = NA_real_),
               fixed = TRUE, regexp = "!anyNA(lower) is not TRUE")
  expect_error(plot_posterior(posterior, upper = "foo"),
               fixed = TRUE, regexp = "is.numeric(upper) is not TRUE")
  expect_error(plot_posterior(posterior, upper = numeric()),
               fixed = TRUE, regexp = "length(upper) > 0 is not TRUE")
  expect_error(plot_posterior(posterior, upper = 1:2),
               fixed = TRUE,
               regexp = "If the 'upper' arg has length > 1, it must be a named numeric vector")
  expect_error(plot_posterior(posterior, upper = c(a=1, a=2)),
               fixed = TRUE, regexp = "!anyDuplicated(names(upper)) is not TRUE")
  expect_error(plot_posterior(posterior, upper = NA_real_),
               fixed = TRUE, regexp = "!anyNA(upper) is not TRUE")
  expect_error(plot_posterior(posterior, lower = 2, upper = 1),
               fixed = TRUE, regexp = "The upper value is less than the lower value")
  expect_error(plot_posterior(posterior, lower = 2, upper = c(m=3, sigma=1)),
               fixed = TRUE,
               regexp = "At least one of the upper values is less than the single lower value that applies to all params.")
  expect_error(plot_posterior(posterior, lower = c(m=3, sigma=1), upper = 2),
               fixed = TRUE,
               regexp = "At least one of the lower values is greater than the single upper value that applies to all params.")
  expect_error(plot_posterior(posterior, lower = c(m=1, sigma=2), upper = c(m=3, sigma=1)),
               fixed = TRUE,
               regexp = "For param sigma a lower value of 2 and an upper value of 1 were specified. Upper values must be greater than lower values.")

})

# Visually confirm that plots look correct? Not part of automated testing.
do_manual_visual_testing <- FALSE
if (do_manual_visual_testing) {

  # Test on example prior & posterior stanfit objects included in mastiff
  posterior <- stan_example_regression$posterior_samples
  prior     <- stan_example_regression$prior_samples
  true_vals <- stan_example_regression$true_values
  plot_posterior(posterior)
  plot_posterior(posterior, true_param_values = true_vals)
  plot_posterior(posterior, prior_samples = prior)
  plot_posterior(posterior, prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, params_desired = c("m", "sigma"))
  plot_posterior(posterior, params_desired = c("m", "sigma"), true_param_values = true_vals)
  plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior)
  plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, transform = list(sigma = log))
  plot_posterior(posterior, transform = list(sigma = log), true_param_values = true_vals)
  plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior)
  plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"))
  plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals)
  plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior)
  plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"))
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"))
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log))
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"))
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior)
  plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals)
  plot_posterior(posterior, prior_samples = prior,
                 lower = c(c=1, m = 2.8, sigma = 0, foo = 4, spam = 2),
                 upper = c(c = 2, m = 3.1, sigma = 1.5))
  plot_posterior(posterior, prior_samples = prior,
                 lower = c(foo = 4,spam = 2)) # ignore unknown params

  # Test on prior & posterior dataframe objects
  posterior_dt <- data.frame(m = stats::rnorm(100),
                             c = stats::rnorm(100, mean = 10),
                             sigma = stats::rlnorm(100))
  prior_dt <- data.frame(m = stats::rnorm(100, sd = 2),
                         c = stats::rnorm(100, mean = 10, sd = 2),
                         sigma = stats::rlnorm(100, sdlog = 2))
  true_vals_dt <- c(m = 0, c = 10, sigma = 1)
  plot_posterior(posterior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior_dt, true_param_values = true_vals_dt, skip_stanfit_to_dt = TRUE)

  # Extra tests for new args
  plot_posterior(posterior_dt, prior_samples = prior_dt, upper = 10, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, prior_samples = prior_dt, lower = 2, skip_stanfit_to_dt = TRUE)
  plot_posterior(posterior_dt, prior_samples = prior_dt, lower = 2, upper = 10, skip_stanfit_to_dt = TRUE)

}
