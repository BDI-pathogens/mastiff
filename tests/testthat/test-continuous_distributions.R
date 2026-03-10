test_that( "distribution.exponential constructs a valid class", {
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

test_that( "distribution.gamma constructs a valid class", {
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

test_that( "distribution.normal constructs a valid class", {
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
