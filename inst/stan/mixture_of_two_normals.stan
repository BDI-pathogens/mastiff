data {

  // Actual data
  int<lower = 0> n;
  vector[n] y;
  int<lower = 0> num_groups;
  array[1 ? num_groups > 0 : 0, n] int<lower = 1, upper = num_groups> groups;

  // Other things to keep fixed over a complete round of sampling: a binary
  // switch to control whether we sample from the prior or the posterior
  // (important to compare the difference), and upper and lower bounds for the
  // priors.
  int<lower = 0, upper = 1> sample_posterior_not_prior;
  real mu_0_lower;
  real<lower = mu_0_lower> mu_0_upper;
  real<lower = mu_0_lower> mu_1_lower;
  real<lower = mu_1_lower> mu_1_upper;
  real<lower = 0> sd_0_lower;
  real<lower = sd_0_lower> sd_0_upper;
  real<lower = 0> sd_1_lower;
  real<lower = sd_1_lower> sd_1_upper;
  real<lower = 0> p_lower;
  real<lower = p_lower, upper = 1> p_upper;
  real<lower = 0> sd_groups_lower;
  real<lower = sd_groups_lower> sd_groups_upper;
}

transformed data {

  if (num_groups == 1) {
    reject("num_groups should not be set to 1: use 0 if there are no",
    "groups i.e. all observations have the same probability of being in",
    "one component of the mixture or the other, or 2 or more if there are 2 or",
    "more groups within the data that differ in this probability.");
  }

  // Are we predicting (specifying regression models for) p? Use the resulting 0
  // or 1 as an extra tensor dimension for
  // the associated parameters (since defining them as a tensor of size 0 in any
  // dimension means they are effectively not created).
  int predict_p = 1 ? num_groups > 0 : 0;
}

parameters {
  real<lower = mu_0_lower, upper = mu_0_upper> mu_0;
  real<lower = max([mu_1_lower, mu_0]), upper = mu_1_upper> mu_1; // Enforce mu_1 > mu_0
  real<lower = sd_0_lower, upper = sd_0_upper> sd_0;
  real<lower = sd_1_lower, upper = sd_1_upper> sd_1;
  real<lower = p_lower,    upper = p_upper>    p;
  vector[num_groups] beta_unscaled_p;
  array[predict_p] real<lower = sd_groups_lower, upper = sd_groups_upper> sd_groups;
}

transformed parameters {

  vector[num_groups] p_by_group;
  vector[n] lp_0; // log( P(data | d = 0) * P(d = 0 | group) )
  vector[n] lp_1; // log( P(data | d = 1) * P(d = 1 | group) )

  if (predict_p) {
    p_by_group = inv_logit(logit(p) + beta_unscaled_p * sd_groups[1]);
  }


  if (sample_posterior_not_prior) {

    // Initialise the lp terms with P(d | group)
    if (predict_p) {
      vector[num_groups] p_by_group_log   = log(p_by_group);
      vector[num_groups] p_by_group_log1m = log1m(p_by_group);
      for (i in 1:n) {
        lp_1[i] = p_by_group_log[groups[1, i]];
        lp_0[i] = p_by_group_log1m[groups[1, i]];
      }
    } else {
      lp_1 = rep_vector(log(p),   n);
      lp_0 = rep_vector(log1m(p), n);
    }

    // Increment the lp terms with P(data | d)
    for (i in 1:n) {
      lp_1[i] += normal_lpdf(y[i] | mu_1, sd_1);
      lp_0[i] += normal_lpdf(y[i] | mu_0, sd_0);
    }
  }

}


model {

  // Priors
  beta_unscaled_p ~ std_normal();
  mu_1 ~ uniform(max([mu_1_lower, mu_0]), mu_1_upper);

  // Likelihood
  if (sample_posterior_not_prior) {
    // P(data | groups) = exp(lp_0) + exp(lp_1) since 0 and 1 are
    // collectively exhaustive and mutually exclusive.
    // Taking the log of that:
    for (i in 1:n) target += log_sum_exp(lp_1[i], lp_0[i]);
  }

}

generated quantities {
  vector[n] prob_is_1;
  for (i in 1:n) prob_is_1[i] = exp(lp_1[i] - log_sum_exp(lp_1[i], lp_0[i]));
}
