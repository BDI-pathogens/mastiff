test_that( "distribution.multivariate class and distribution", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    means <- c( 1, -1 )
    covariance   <- matrix( c( 1, 1, 1, 2 ), nrow = 2 )
    expect_no_error( { X <- distribution.multivariate.normal( means * 3, covariance * 2 ) } )
    expect_equal( X$params$means, means * 3 )
    expect_equal( X$params$covariance, covariance * 2 ) 
    
    # check update
    expect_no_error( { X$params$means <- means } ) 
    expect_equal( X$params$means, means )
    expect_no_error( { X$params$covariance <- covariance } ) 
    expect_equal( X$params$covariance, covariance ) 
    
    # check univariate moments
    expect_no_error( { samples <- X$r( n ) } )
    for( idx in 1:length( means ) ) {
      expect_equal( mean( samples[ , idx ] ), X$mean[ idx ], tolerance = tol  )
      expect_equal( sd( samples[ , idx ] ), X$sd[ idx ], tolerance = tol  )
    }

    # check univariate cumulative distribution functions
    expect_no_error( { q <- X$p( samples ) } )
    for( idx in 1:length( means ) ) {
      expect_lt( max( abs( sort( q[ , idx]) -( 1:nrow( q ) ) / nrow(q)) ), tol )
    }
    
    # check quantile function    
    expect_no_error( { p_inv <- X$q( q ) } )  
    expect_lt( max( abs( p_inv - samples ) ), 1e-10 )
      
    # check correlation
    rho <- covariance[1,2] / sqrt( covariance[1,1] * covariance[2,2])
    expect_equal( cor( samples[ , 1], samples[ , 2 ] ), rho, tolerance = tol )
    
    # check update of correlation  
    X$set_uniform_correlation( 0.5 )
    expect_no_error( { samples <- X$r( n ) } )
    expect_equal( cor( samples[ , 1], samples[ , 2 ] ), 0.5, tolerance = tol )
  } )
} )