
stan_model <- "
data {
  // actual data
  int<lower = 0> N;
  vector[N] x;
  vector[N] y;

  // things to keep fixed over one round of inference
  int<lower = 0, upper = 1> sample_posterior_not_prior;
  real m_lower;
  real<lower = m_lower> m_upper;
  real c_lower;
  real<lower = c_lower> c_upper;
  real<lower = 0> sigma_lower;
  real<lower = sigma_lower> sigma_upper;
}

parameters {
  real<lower = m_lower, upper = m_upper> m;
  real<lower = c_lower, upper = c_upper> c;
  real<lower = sigma_lower, upper = sigma_upper> sigma;
}

model {
  if (sample_posterior_not_prior) y ~ normal(m * x + c, sigma);
}
"

stan_model_compiled <- rstan::stan_model(model_code = stan_model)

x <- 1:20
m <- 3
c <- 2
sigma <- 1

y <- rnorm(length(x), mean = m * x + c, sd = sigma)

stan_input_posterior <- list(
  N = length(x),
  x = x,
  y = y,
  m_lower = 0,
  m_upper = 6,
  c_lower = 0,
  c_upper = 4,
  sigma_lower = 0,
  sigma_upper = 2,
  sample_posterior_not_prior = 1
)
stan_input_prior <- stan_input_posterior
stan_input_prior$sample_posterior_not_prior <- 0

posterior <- rstan::sampling(stan_model_compiled,
                             data = stan_input_posterior)
prior <- rstan::sampling(stan_model_compiled,
                         data = stan_input_prior)

stan_example_regression <- list(
  posterior_samples = posterior,
  prior_samples = prior,
  true_values = c("m" = m,
                  "c" = c,
                  "sigma" = sigma)
)

usethis::use_data(stan_example_regression, overwrite = TRUE)
