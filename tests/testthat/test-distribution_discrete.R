test_that( "Default $p() and $q() return the correct CDF and quantile function on distribution.discrete.class", {
  # Define a temporary class for binomial distribution with $p() and $r()
  # defined but not $p() or $q()
  partial_discrete.class <- R6.class(
    classname = "distribution.discrete.tmp.class",
    inherit   = distribution.discrete.class,
    interfaces = list( distribution.interface ),
    private   = list(
      .name    = "Binomial",
      .param_names = c( "size", "prob" ),
      .check_params = function( params ){
        return( NULL )
      }
    ),
    public = list(
      initialize = function( size, prob ){
        private$.check_params( list( size = size,
                                     prob = prob ) )
        super$initialize( support = c( 0, size ) )
        self$params <- list( size = size,
                             prob = prob )
      },
      d = function( x, log = FALSE ){
        stats::dbinom( x, size = private$.params$size, prob = private$.params$prob,
                       log = log )
      },
      r = function( n ){
        stats::rbinom( n, size = private$.params$size, prob = private$.params$prob )
      }
    )
  )
  
  size <- 50
  prob <- 0.05
  tol <- 1e-10
  
  test_class <- partial_discrete.class$new( size = size, prob = prob )
  binom_class <- distribution.discrete.binomial.class$new( size = size, prob = prob )
  
  
  q <- seq( -1, size + 1, by = 1 )
  expect_equal( test_class$p( q, lower.tail = TRUE, log.p = FALSE ),
                binom_class$p( q, lower.tail = TRUE, log.p = FALSE ),
                tolerance = tol )
  
  
  # Test only up to 1% tails of the distribution
  x_max <- stats::qbinom( p = 0.99, size = size, prob = prob )
  p <- binom_class$p( 0 : x_max )
  expect_equal( test_class$q( p, lower.tail = TRUE, log.p = FALSE ),
                binom_class$q( p, lower.tail = TRUE, log.p = FALSE ),
                tolerance = tol )
})

test_that( "distribution.binomial constructs a valid class", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    X <- distribution.binomial( size = 10,
                                prob = 0.5 )
    
    # Test that density is correct for initial rate parameter
    expect_equal( X$d( x = 0 ), 0.5^10 )
    expect_equal( mean( X$r( n ) ), X$mean, tolerance = tol )
    
    # Test that $params can be updated via named list
    expect_no_error( X$params <- list( size = 5,
                                       prob = 0.8 ) )
    expect_equal({
      X$params <- list( size = 5,
                        prob = 0.8 )
      X$d( x = 0 )
    }, 0.2^5 )
    expect_equal({
      X$params <- list( size = 5,
                        prob = 0.8 )
      mean( X$r( n ) )
    }, X$mean, tolerance = tol )
    
    # Test that elements of $params can be updated by name
    expect_no_error( X$params$size <- 1 )
    expect_no_error( X$params$prob <- 0 )
    
    # Test that invalid values of $params fail (via private$.check_params())
    expect_error( X$params$size <- -1 )
    expect_error( X$params$size <- 0.5 )
    expect_error( X$params$size <- 'a' )
    expect_error( X$params$prob <- -1 )
    expect_error( X$params$prob <- 2 )
    expect_error( X$params$prob <- 'a' )
    
    # Test that incorrectly named list $params is rejected
    expect_error( X$params <- list( foo = 1 ) )
    expect_error( X$params <- list( 1 ) )
  })
})

test_that( "distribution.poisson constructs a valid class", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    X <- distribution.poisson( lambda = 1 )
    
    # Test that density is correct for initial rate parameter
    expect_equal( X$d( x = 0 ), exp( -1 ) )
    expect_equal( mean( X$r( n ) ), X$mean, tolerance = tol )
    
    # Test that $params can be updated via named list
    expect_no_error( X$params <- list( lambda = 5 ) )
    expect_equal({
      X$params <- list( lambda = 5 )
      X$d( x = 0 )
    }, exp( -5 ) )
    expect_equal({
      X$params <- list( lambda = 5 )
      mean( X$r( n ) )
    }, X$mean, tolerance = tol )
    
    # Test that elements of $params can be updated by name
    expect_no_error( X$params$lambda <- 1 )
    
    # Test that invalid values of $params fail (via private$.check_params())
    expect_error( X$params$lambda <- -1 )
    expect_error( X$params$lambda <- 'a' )
    
    # Test that incorrectly named list $params is rejected
    expect_error( X$params <- list( foo = 1 ) )
    expect_error( X$params <- list( 1 ) )
  })
})

test_that( "distribution.negative_binomial constructs a valid class", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    X <- distribution.negative_binomial( size = 10, prob = 0.5 )
    
    # # Test that density is correct for initial rate parameter
    expect_equal( X$d( x = 0 ), 0.5^10 )
    expect_equal( mean( X$r( n ) ), X$mean, tolerance = tol )
    
    # Test that $params can be updated via named list
    expect_no_error( X$params <- list( size = 10,
                                       prob = 0.5 ) )
    expect_no_error( X$params <- list( size = 10,
                                       mu   = 10 ) )
    expect_no_error( X$params <- list( size = 10,
                                       prob = 0.5,
                                       mu   = 10 ) )
    
    #Test that $params throws an error is prob and mu are both set and are
    #inconsistent
    expect_error( X$params <- list( size = 10,
                                    prob = 0.5,
                                    mu   = 0 ) )
    
    # Test that $params throws an error if size is not set
    expect_error( X$params <- list( prob = 0.5 ) )
    
    # Test that invalid values of $params fail (via private$.check_params())
    expect_error( X$params$prob <- -1 )
    expect_error( X$params$prob <- 'a' )
    expect_error( X$params$mu <- -1 )
    expect_error( X$params$mu <- 'a' )
    expect_error( X$params$size <- -1 )
    expect_error( X$params$size <- 0.5 )
    expect_error( X$params$size <- 'a' )
  })
})

test_that( "distribution.point_mass constructs a valid class", {
  n <- 1e5
  tol <- 3 / sqrt( n )
  
  X <- distribution.point_mass( value = 1 )
  
  # Test that density is correct for initial rate parameter
  expect_equal( X$d( x = 0 ), 0 )
  expect_equal( X$d( x = 1 ), 1 )
  
  # Test that $params can be updated via named list
  expect_no_error( X$params <- list( value = 5 ) )
  expect_equal( { X$params <- list( value = 5 )
  X$d( x = 5 ) }, 1 )
  
  # Test that elements of $params can be updated by name
  expect_no_error( X$params$value <- 5 )
  
  # Test that invalid values of $params fail (via private$.check_params())
  expect_error( X$params$value <- 'a' )
  expect_error( X$params$value <- FALSE )
  
  # Test that incorrectly named list $params is rejected
  expect_error( X$params <- list( foo = 1 ) )
  expect_error( X$params <- list( 1 ) )
})
