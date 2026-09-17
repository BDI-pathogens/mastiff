test_that( "distribution.multivariate_normal class and distribution", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    means <- c( 1, -1 )
    covariance   <- matrix( c( 1, 1, 1, 2 ), nrow = 2 )
    expect_no_error( { X <- distribution.multivariate_normal( means * 3, covariance * 2 ) } )
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

test_that( "distribution.copula check univariate distributions", {
  withr::with_seed( 123, {
    n <- 1e5

    # univariate distributions for the Gaussian copula
    dist <- list( 
      distribution.binomial( 1e3, 0.4), 
      distribution.binomial( 1e3, 0.5),
      distribution.binomial( 1e3, 0.6)
    )
    
    expect_no_error( { dist_gc <- distribution.copula_gaussian( dist, 0.5 ) } )
    sample_gc <- dist_gc$r( n )
   
    # check the copula univariate distributions are the same as the underlying
    # using the Kolmogorov-Smirnov test for samples
    # suppress tie warning message 
    for( idx in 1:length( dist ) ) {
      sample_uv <- dist[[ idx ]]$r( n )
      suppressWarnings( { kst <- stats::ks.test( sample_gc[,idx], sample_uv ) } )
      expect_gt( kst$"p.value", 0.01  )
    }
    
    # check moments
    means <- dist_gc$mean
    vars  <- dist_gc$var
    sds   <- dist_gc$sd
    for( idx in 1:length( dist ) ) {
      expect_equal( means[ idx ], dist[[idx]]$mean )  
      expect_equal( vars[ idx ],  dist[[idx]]$var )  
      expect_equal( sds[ idx ],   dist[[idx]]$sd )  
    } 
  } )
} )

test_that( "distribution.copula check correlation", {
  withr::with_seed( 123, {
    n <- 1e5
    rho <- 0.5

    # univariate distributions for the Gaussian copula
    dist <- list( 
      distribution.binomial( 1e3, 0.4), 
      distribution.binomial( 1e3, 0.5),
      distribution.binomial( 1e3, 0.6)
    )
    
    expect_no_error( { dist_gc <- distribution.copula_gaussian( dist, rho ) } )
    sample_gc <- dist_gc$r( n )
    
    # check the correlation is approximately the copula correlation when the
    # univariate distributions are approximately Gaussian
    for( idx in 1:(length( dist ) - 1 ) ) 
      for( jdx in (idx+1):length( dist ) ) {
        expect_lt( abs( rho - cor( sample_gc[ , idx ], sample_gc[ , jdx ] ) ), 0.01 )
      }
  } )
} )

test_that( "distribution.multivariate_t class and distribution", {
  withr::with_seed( 123, {
    n <- 1e5
    tol <- 3 / sqrt( n )
    
    means <- c( 1, -1 )
    scale_matrix <- matrix( c( 1, 1, 1, 2 ), nrow = 2 )
    df <- 6
    expect_no_error( { X <- distribution.multivariate_t( means * 3, scale_matrix * 2, df * 4  ) } )
    expect_equal( X$params$means, means * 3 )
    expect_equal( X$params$scale_matrix, scale_matrix * 2 ) 
    expect_equal( X$params$df,df * 4 ) 
    
    # check update
    expect_no_error( { X$params$means <- means } ) 
    expect_equal( X$params$means, means )
    expect_no_error( { X$params$scale_matrix <- scale_matrix } ) 
    expect_equal( X$params$scale_matrix, scale_matrix ) 
    expect_no_error( { X$params$df <- df } ) 
    expect_equal( X$params$df, df ) 
    
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
    expect_lt( max( abs( p_inv - samples ) ), 1e-9 )
    
    # check correlation
    rho <- scale_matrix[1,2] / sqrt( scale_matrix[1,1] * scale_matrix[2,2])
    expect_equal( cor( samples[ , 1], samples[ , 2 ] ), rho, tolerance = tol )
    
    # check update of correlation  
    X$set_uniform_correlation( 0.5 )
    expect_no_error( { samples <- X$r( n ) } )
    expect_equal( cor( samples[ , 1], samples[ , 2 ] ), 0.5, tolerance = tol )
  } )
} )

test_that( "t copula check univariate distributions", {
  withr::with_seed( 123, {
    n <- 1e5
    
    # univariate distributions for the t- copula
    dist <- list( 
      distribution.binomial( 1e3, 0.4), 
      distribution.binomial( 1e3, 0.5),
      distribution.binomial( 1e3, 0.6)
    )
    
    expect_no_error( { dist_tc <- distribution.copula_t( dist, 0.5, 6 ) } )
    sample_tc <- dist_tc$r( n )
    
    # check the copula univariate distributions are the same as the underlying
    # using the Kolmogorov-Smirnov test for samples
    # suppress tie warning message 
    for( idx in 1:length( dist ) ) {
      sample_uv <- dist[[ idx ]]$r( n )
      suppressWarnings( { kst <- stats::ks.test( sample_tc[,idx], sample_uv ) } )
      expect_gt( kst$"p.value", 0.01  )
    }
    
    # check moments
    means <- dist_tc$mean
    vars  <- dist_tc$var
    sds   <- dist_tc$sd
    for( idx in 1:length( dist ) ) {
      expect_equal( means[ idx ], dist[[idx]]$mean )  
      expect_equal( vars[ idx ],  dist[[idx]]$var )  
      expect_equal( sds[ idx ],   dist[[idx]]$sd )  
    } 
  } )
} )

test_that( "t check correlation", {
  withr::with_seed( 123, {
    n <- 1e5
    rho <- 0.5
    
    # univariate distributions for thet-copula
    # large size so approximately Gaussian
    dist <- list( 
      distribution.binomial( 1e3, 0.4), 
      distribution.binomial( 1e3, 0.5),
      distribution.binomial( 1e3, 0.6)
    )
    
    # large degrees of freedom, so approximately Gaussian
    expect_no_error( { dist_tc <- distribution.copula_t( dist, rho, 50 ) } )
    sample_tc <- dist_tc$r( n )
    
    # check the correlation is approximately the copula correlation when the
    # univariate distributions are approximately Gaussian
    for( idx in 1:(length( dist ) - 1 ) ) 
      for( jdx in (idx+1):length( dist ) ) {
        expect_lt( abs( rho - cor( sample_tc[ , idx ], sample_tc[ , jdx ] ) ), 0.01 )
      }
  } )
} )


