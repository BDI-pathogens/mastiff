test_that( "Default $p() and $q() return the correct CDF and quantile function on distribution.discrete.class", {
  # Define temporary continuous distribution classes with $d() and $r()
  # defined but not $p() or $q() to test default $p() and $q()
  tol <- 1e-6
  rate <- 0.1
  
  test_p_q <- function( test_class,
                        dist_class,
                        min_quantile = 0.01 ){
    # Test up to 1% tails of the distribution
    q_range <- dist_class$q( c( min_quantile, 1 - min_quantile ) )
    q <- seq( q_range[ 1 ], q_range[ 2 ], length.out = 20 )
    p <- dist_class$p( q )
    
    expect_equal( test_class$p( q, lower.tail = TRUE, log.p = FALSE ),
                  dist_class$p( q, lower.tail = TRUE, log.p = FALSE ),
                  tolerance = tol )
    expect_equal( test_class$q( p, lower.tail = TRUE, log.p = FALSE ),
                  dist_class$q( p, lower.tail = TRUE, log.p = FALSE ),
                  tolerance = tol )
  }
  
  # Finite support -- test with uniform distribution
  unif_min <- 0
  unif_max <- 10
  unif_test_class <- distribution.continuous.uniform.class <- utils.class(
    classname = "distribution.continuous.uniform.class",
    inherit   = distribution.continuous.class,
    private   = list(
      .name    = "Uniform",
      .param_names = c( "min", "max" ),
      .check_params = function( params ) return( NULL )
    ),
    public = list(
      initialize = function( min = 0, max = 1 ){
        super$initialize( support = c( min, max ) )
        self$params <- list( min = min, max = max )
      },
      d = function( x, log = FALSE ) stats::dunif( x, min = private$.params$min, max = private$.params$max, log = log ),
      r = function( n ) stats::runif( n, min = private$.params$min, max = private$.params$max ) )
  )$new( min = unif_min, max = unif_max )
  unif_class <- distribution.uniform( min = unif_min, max = unif_max )
  test_p_q( unif_test_class,
            unif_class )
  
  # Support [0, Inf) -- test via exponential distribution
  exp_rate <- 0.1
  exp_test_class <- utils.class(
    classname = "distribution.continuous.tmp.class",
    inherit   = distribution.continuous.class,
    interfaces = list( distribution.interface ),
    private   = list(
      .name    = "Exponential",
      .param_names = c( "rate" ),
      .check_params = function( params ) return( NULL )
    ),
    public = list(
      initialize = function( rate = 1 ){
        super$initialize( support = c( 0, Inf ) )
        self$params <- list( rate = rate )
      },
      d = function( x, log = FALSE ) stats::dexp( x, rate = private$.params$rate, log = log ),
      r = function( n ) stats::rexp( n, rate = private$.params$rate ) )
  )$new( rate = exp_rate )
  exp_class <- distribution.exponential( rate = exp_rate )
  
  test_p_q( exp_test_class, exp_class )
  
  # Support (-Inf, Inf) -- test via normal distribution
  norm_mean <- 0
  norm_sd <- 10
  norm_test_class <- utils.class(
    classname = "distribution.continuous.normal.class",
    inherit   = distribution.continuous.class,
    private   = list(
      .name    = "normal",
      .param_names = c( "mean",
                        "sd" ),
      .check_params = function( params ) return( NULL )
    ),
    public = list(
      initialize = function( mean, sd ){
        super$initialize( support = c( -Inf, Inf ) )
        self$params <- list( mean = mean, sd   = sd )
      },
      d = function( x, log = FALSE ) stats::dnorm( x, mean = private$.params$mean, sd = private$.params$sd, log = log ),
      r = function( n ) stats::rnorm( n, mean = private$.params$mean, sd = private$.params$sd ) )
  )$new( mean = norm_mean, sd = norm_sd )
  norm_class <- distribution.normal( mean = norm_mean, sd = norm_sd )
  
  ## Due to numerical instability of integrate on normal density, test quantile
  ## function using pnorm
  
  q_range <- norm_class$q( c( 0.01, 0.99 ) )
  q <- seq( q_range[ 1 ], q_range[ 2 ], length.out = 20 )
  p <- norm_class$p( q )
  
  expect_equal( norm_test_class$p( q, lower.tail = TRUE, log.p = FALSE ),
                norm_class$p( q, lower.tail = TRUE, log.p = FALSE ),
                tolerance = tol )
  
  norm_test_class <- utils.class(
    classname = "distribution.continuous.normal.class",
    inherit   = distribution.continuous.class,
    private   = list(
      .name    = "normal",
      .param_names = c( "mean",
                        "sd" ),
      .check_params = function( params ) return( NULL )
    ),
    public = list(
      initialize = function( mean, sd ){
        super$initialize( support = c( -Inf, Inf ) )
        self$params <- list( mean = mean, sd   = sd )
      },
      d = function( x, log = FALSE ) stats::dnorm( x, mean = private$.params$mean, sd = private$.params$sd, log = log ),
      p = function( q, lower.tail = TRUE, log.p = FALSE ) stats::pnorm( q, mean = private$.params$mean, sd = private$.params$sd, lower.tail = lower.tail, log.p = log.p ),
      r = function( n ) stats::rnorm( n, mean = private$.params$mean, sd = private$.params$sd ) )
  )$new( mean = norm_mean, sd = norm_sd )
  expect_equal( norm_test_class$q( p, lower.tail = TRUE, log.p = FALSE ),
                norm_class$q( p, lower.tail = TRUE, log.p = FALSE ),
                tolerance = tol )
})

test_that( "distribution.exponential constructs a valid class", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    X <- distribution.exponential( rate = 1 )
    
    # Test that density is correct for initial rate parameter
    expect_equal( X$d( x = 0 ), 1 )
    expect_equal( mean( X$r( n ) ), X$mean, tolerance = tol )
    
    # Test that $params can be updated via named list
    expect_no_error( X$params <- list( rate = 10 ) )
    expect_equal({
      X$params <- list( rate = 10 )
      X$d( x = 0 )
    }, 10 )
    expect_equal({
      X$params <- list( rate = 10 )
      mean( X$r( n ) )
    }, X$mean, tolerance = tol )
    
    # Test that elements of $params can be updated by name
    expect_no_error( X$params$rate <- 1 )
    
    # Test that invalid values of $params fail (via private$.check_params())
    expect_error( X$params$rate <- -1 )
    expect_error( X$params <- list( rate = -1 ) )
    expect_error( X$params$rate <- 'a' )
    expect_error( X$params <- list( rate = 'a' ) )
    
    # Test that incorrectly named list $params is rejected
    expect_error( X$params <- list( foo = 1 ) )
    expect_error( X$params <- list( 1 ) )
    expect_error( X$params <- list( rate = 1,
                                    foo = 1 ) )
  })
})

test_that( "distribution.gamma constructs a valid class", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    X <- distribution.gamma( shape = 1, rate = 1, scale = 1 )
    
    # Test that distribution can be initialised with exactly one of rate or scale
    expect_no_error( X <- distribution.gamma( shape = 1, rate = 0.5 ) )
    expect_no_error( X <- distribution.gamma( shape = 1, scale = 0.5 ) )
    
    # ...and initialisation fails if rate != 1 / scale
    expect_error( X <- distribution.gamma( shape = 1, rate = 1, scale = 100 ) )
    
    # Test that density is correct for initial rate parameter
    expect_equal( X$d( x = 1 ),
                  X$params$rate * exp( - X$params$rate ) )
    expect_equal( mean( X$r( n ) ), X$mean, tolerance = tol )
    
    # Test that $params can be updated via named list
    expect_no_error( X$params <- list( shape = 0.5,
                                       rate = 10 ) )
    expect_equal({
      X$params <- list( shape = 0.5,
                        rate = 10 )
      mean( X$r( n ) )
    }, X$mean, tolerance = tol )
    
    # Test that elements of $params can be updated by name
    expect_no_error( X$params$rate <- 10 )
    expect_no_error( X$params$scale <- 10 )
    
    # Test that invalid values of $params fail (via private$.check_params())
    expect_error( X$params$shape <- -1 )
    expect_error( X$params$shape <- 'a' )
    expect_error( X$params$rate <- -1 )
    expect_error( X$params$rate <- 'a' )
    expect_error( X$params$scale <- -1 )
    expect_error( X$params$scale <- 'a' )
    
    
    # Test that incorrectly named list $params is rejected
    expect_error( X$params <- list( foo = 1 ) )
    expect_error( X$params <- list( 1 ) )
    expect_error( X$params <- list( rate = 1,
                                    foo = 1 ) )
  })
})

test_that( "distribution.normal constructs a valid class", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    X <- distribution.normal( mean = 0, sd = 1 )
    
    # Test that density is correct for initial rate parameter
    expect_equal( X$d( x = 0 ), 1 / sqrt( 2 * pi ) )
    expect_equal( mean( X$r( n ) ), X$mean, tolerance = tol )
    
    # Test that $params can be updated via named list
    expect_no_error( X$params <- list( mean = 1,
                                       sd   = 1 ) )
    expect_equal({
      X$params <- list( mean = 10,
                        sd = 1 )
      X$d( x = 10 )
    }, 1 / sqrt( 2 * pi ) )
    
    # Test that elements of $params can be updated by name
    expect_no_error( X$params$mean <- 1 )
    expect_no_error( X$params$sd <- 10 )
    
    # Test that invalid values of $params fail (via private$.check_params())
    expect_error( X$params$mean <- 'a' )
    expect_error( X$params$sd <- -1 )
    expect_error( X$params$sd <- 'a' )
    
    # Test that incorrectly named list $params is rejected
    expect_error( X$params <- list( foo = 1 ) )
    expect_error( X$params <- list( 1 ) )
    expect_error( X$params <- list( rate = 1,
                                    foo = 1 ) )
  })
})
