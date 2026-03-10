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