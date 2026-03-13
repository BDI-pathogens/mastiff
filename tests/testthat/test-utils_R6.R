test_that( "utils.uniroot.vectorised() returns the same root as stats::uniroot()", {
  # Quartic function with roots at +/- 1 and +/- sqrt( 2 )
  test_func <- function( x ) ( x - 1 ) * ( x + 1 ) * ( x^2 - 2 )
  
  intervals <- list( c( - 2, -1.2 ),
                     c( -1.2, 0 ),
                     c( 0, 1.2 ),
                     c( 1.2, 2 ) )
  stats_uniroot <- numeric( length( intervals ) )
  for ( idx in seq_along( intervals ) ){
    stats_uniroot[ idx ] <- stats::uniroot( test_func,
                                            interval = intervals[[ idx]] )$root
  }
  
  interval_lower <- sapply( intervals, min, na.rm = TRUE )
  interval_upper <- sapply( intervals, max, na.rm = TRUE )
  
  mastiff_uniroot <- utils.uniroot.vectorized(
    f = test_func,
    lower = interval_lower,
    upper = interval_upper )
  
  expect_true( TRUE )
})