# Visually confirm that plots look correct? Not part of automated testing.
do_manual_visual_testing <- FALSE

# TODO: test for duplicate names in named vector/list args

# Test each of the 2^5
# possibilities for the 5 optional arguments (i.e. specifying them or not).
# (Transforming a param without renaming it makes no sense, nor does renaming
# it without transforming it (as below at least), but just to test the idea.)

# Test on example prior & posterior stanfit objects included in mastiff
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

# Test on prior & posterior data.table objects
posterior <- data.table::data.table(m = stats::rnorm(100),
                        c = stats::rnorm(100, mean = 10),
                        sigma = stats::rlnorm(100))
prior <- data.table::data.table(m = stats::rnorm(100, sd = 2),
                    c = stats::rnorm(100, mean = 10, sd = 2),
                    sigma = stats::rlnorm(100, sdlog = 2))
true_vals <- c(m = 0, c = 10, sigma = 1)
expect_no_error(plot_posterior(posterior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE))
expect_no_error(plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE))


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

# Test on prior & posterior data.table objects
posterior <- data.table::data.table(m = stats::rnorm(100),
                                    c = stats::rnorm(100, mean = 10),
                                    sigma = stats::rlnorm(100))
prior <- data.table::data.table(m = stats::rnorm(100, sd = 2),
                                c = stats::rnorm(100, mean = 10, sd = 2),
                                sigma = stats::rlnorm(100, sdlog = 2))
true_vals <- c(m = 0, c = 10, sigma = 1)
plot_posterior(posterior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), true_param_values = true_vals, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, skip_stanfit_to_dt = TRUE)
plot_posterior(posterior, labels = c(sigma = "log(sigma)"), transform = list(sigma = log), params_desired = c("m", "sigma"), prior_samples = prior, true_param_values = true_vals, skip_stanfit_to_dt = TRUE)

}
