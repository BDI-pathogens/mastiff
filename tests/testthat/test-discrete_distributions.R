test_that( "distribution.binomial constructs a valid class", {
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

test_that( "distribution.poisson constructs a valid class", {
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

test_that( "distribution.negative_binomial constructs a valid class", {
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
