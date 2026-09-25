test_that("Truncation to an interval [a,b] updates support", {
  # Generate a standard normal random variable X ~ N(0, 1)
  truncated_normal <- distribution.truncated.class$new(
    distribution = distribution.normal(mean = 0, sd = 1)
  )
  
  # Initialised variable has full support of a Normal distribution
  expect_equal(truncated_normal$support, c(-Inf, Inf))
  expect_equal(truncated_normal$.__enclos_env__$private$.normalisation_constant, 1)
  
  # Update lower truncation -> X | X >= 0
  #   - Support [0, Inf)
  #   - Normalisation constant 0.5
  expect_no_error(truncated_normal$t0 <- 0)
  expect_equal(truncated_normal$support, c(0, Inf))
  expect_equal(truncated_normal$.__enclos_env__$private$.normalisation_constant, 0.5)
  
  # Update upper truncation -> X | X \in [0, qnorm(0.75)]
  #   - Support [0,qnorm(0.75)]
  #   - Normalisation constant 0.25
  
  expect_no_error(truncated_normal$t1 <- qnorm(0.75))
  expect_equal(truncated_normal$support, c(0, qnorm(0.75)))
  expect_equal(truncated_normal$.__enclos_env__$private$.normalisation_constant, 0.25)
  
  # Remove lower truncation -> X | X <= qnorm(0.75)
  #   - Support (-Inf, qnorm(0.75))
  #   - Normalisation constant 0.75
  expect_no_error(truncated_normal$t0 <- -Inf)
  expect_equal(truncated_normal$support, c(-Inf, qnorm(0.75)))
  expect_equal(truncated_normal$.__enclos_env__$private$.normalisation_constant, 0.75)
})

test_that("Warnings and errors are thrown correctly for truncated support values", {
  expect_no_error(
    truncated_exponential <- distribution.truncated.class$new(
      distribution = distribution.exponential()
    )
  )
  
  # Truncation outside the support of a distribution gives a warning
  expect_warning(
    truncated_exponential$t0 <- -1
  )
  
  # Truncation to an interval with (numerically) 0 mass gives an error
  expect_error({
    truncated_exponential$t0 <- 0
    truncated_exponential$t1 <- 0
  })
  
  # Truncation to an interval with very small mass gives a warning
  expect_warning({
    truncated_exponential$t1 <- 10 + 1e-6
    truncated_exponential$t0 <- 10
  })
})

test_that("$d() returns 0 outside of the truncated support", {
  expect_no_error(truncated_normal <- distribution.truncated.class$new(
    distribution = distribution.normal(mean = 0, sd = 1),
    t0 = -0.5,
    t1 = 0.5
  ))
})

test_that("$p() returns values in [0,1] and has inverse $q()", {
  # Generate a truncated normal random variable X ~ N(0, 1) | X \in [-0.5, 0.5]
  expect_no_error(truncated_normal <- distribution.truncated.class$new(
    distribution = distribution.normal(mean = 0, sd = 1),
    t0 = -0.5,
    t1 =  0.5
  ))
  
  ## Sanity check that t0, t1 have been set by the class initialisation
  expect_equal(truncated_normal$support, c(-0.5, 0.5))
  q <- seq(-1, 1, by = 0.05)
  
  # log.p = FALSE 
  for (lower.tail in c(TRUE, FALSE)){
    
    expect_no_error(p <- truncated_normal$p(q, lower.tail = lower.tail, log.p = FALSE))
    
    ## All CDF values fall in [0,1]
    expect_all_true(p <= 1)
    expect_all_true(p >= 0)
    
    ## All CDF values outside of the truncated support are 0/1
    expect_all_equal(p[q <= -0.5], as.numeric(!lower.tail))
    expect_all_equal(p[q >=  0.5], as.numeric( lower.tail))
    
    ## $q() is the correct inverse for p \in (0, 1)
    expect_equal(truncated_normal$q(p[p > 0 & p < 1], lower.tail = lower.tail),
                 q[p > 0 & p < 1])
  }
  
  # log.p = TRUE
  for (lower.tail in c(TRUE, FALSE)){
    expect_no_error(p <- truncated_normal$p(q, lower.tail = lower.tail, log.p = TRUE))
    
    ## All CDF values fall in (-Inf, 0]
    expect_all_true(p <= 0)
    
    ## All CDF values outside of the truncated support are 0/1
    expect_all_equal(p[q <= -0.5], ifelse(lower.tail, -Inf, 0))
    expect_all_equal(p[q >=  0.5], ifelse(lower.tail, 0, -Inf))
    
    ## $q() is the correct inverse for p \in (-Inf, 0)
    expect_equal(truncated_normal$q(p[is.finite(p) & p < 0], lower.tail = lower.tail, log.p = TRUE),
                 q[is.finite(p) & p < 0])
  }
})

test_that("$q() returns values in truncated support and has inverse $p()", {
  # Generate a truncated normal random variable X ~ N(0, 1) | X \in [-0.5, 0.5]
  expect_no_error(truncated_normal <- distribution.truncated.class$new(
    distribution = distribution.normal(mean = 0, sd = 1),
    t0 = -0.5,
    t1 =  0.5
  ))
  
  ## Sanity check that t0, t1 have been set by the class initialisation
  expect_equal(truncated_normal$support, c(-0.5, 0.5))
  
  # log.p = FALSE 
  p <- seq(0, 1, by = 0.05)
  for (lower.tail in c(TRUE, FALSE)){
    expect_no_error(q <- truncated_normal$q(p, lower.tail = lower.tail, log.p = FALSE))
    
    ## All quantiles fall in truncated support
    expect_all_true(q >= truncated_normal$support[1])
    expect_all_true(q <= truncated_normal$support[2])
    
    ## $p() is the inverse for q
    expect_equal(truncated_normal$p(q, lower.tail = lower.tail),
                 p)
  }
  
  # log.p = TRUE
  p <- log(p)
  for (lower.tail in c(TRUE, FALSE)){
    expect_no_error(q <- truncated_normal$q(p, lower.tail = lower.tail, log.p = TRUE))
    
    ## All quantiles fall in truncated support
    expect_all_true(q >= truncated_normal$support[1])
    expect_all_true(q <= truncated_normal$support[2])
    
    ## $p() is the inverse for q
    expect_equal(truncated_normal$p(q, lower.tail = lower.tail),
                 exp(p))
  }
})

test_that("$r() returns values in truncated support", {
  expect_no_error(truncated_beta <- distribution.truncated.class$new(
    distribution = distribution.beta(
      alpha = 0.5,
      beta = 1.5
    ),
    t0 = 0.2,
    t1 = 0.8
  ))
  
  expect_no_error(r <- truncated_beta$r(1e5))
  expect_gte(min(r), truncated_beta$t0)
  expect_lte(max(r), truncated_beta$t1)
})

