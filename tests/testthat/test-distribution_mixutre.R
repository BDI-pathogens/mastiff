library( mastiff )
library( testthat )

test_that( "Test mixture distribution", {
  
  all_dists <- list( 
    list(
      distribution.gamma( 2,1 ),
      distribution.gamma( 2,2 ) 
    ),
    list(
      distribution.normal( 0,1 ),
      distribution.normal( 1,2 ) 
    )
  )
  all_params <- list( 
    list( 
      list( shape = 1, rate = 2 ),
      list( shape = 3, rate = 1 )
    ),
    list( 
      list( mean = 1, sd = 2 ),
      list( mean = -1, sd = 0.5 )
    )
  )
  
  for( ddx in 1:length( all_dists ) ) {
    dists <- all_dists[[ ddx ]]
    new_params <- all_params[[ ddx ]]
    
    weights <- rep( 1, length( dists ) ) / length( dists )
    expect_no_error( mix <- distribution.mixture( dists, weights ) )
    expect_equal( mix$n_distributions, length( weights ) )
    expect_equal( mix$weights, weights ) 
    expect_equal( mix$support, dists[[1]]$support )
    
    for( idx in 1:length( weights ) ) {
      # check distributions as intialised and updatable
      expect_equal( data.table::address( mix$distributions[[idx]] ),
                    data.table::address( dists[[idx]] ) )
      expect_no_error( mix$distributions[[idx]]$params <- new_params[[ idx ]] )
      for( name in names( new_params[[ idx ]] ) )
        expect_equal( mix$distributions[[idx]]$params[[name]], new_params[[ idx ]][[name]] )
      
      # check weighting just one returns the correct distribution functions
      weights      <- rep( 0, mix$n_distributions )
      weights[idx] <- 1
      expect_no_error( mix$weights <- weights )
      expect_equal( mix$weights, weights )
      expect_equal( mix$d( c(1,2) ),       mix$distributions[[idx]]$d( c(1,2) ) )
      expect_equal( mix$p( c(0.25,0.75) ), mix$distributions[[idx]]$p( c(0.25,0.75) ) )
      expect_equal( mix$q( c(0.25,0.75) ), mix$distributions[[idx]]$q( c(0.25,0.75) ) )
      
      # add log version
      expect_equal( mix$d( c(1,2), log = T ), mix$distributions[[idx]]$d( c(1,2), log = T ) )
      expect_equal( mix$p( c(1,2), log = T ), mix$distributions[[idx]]$p( c(1,2), log = T ) )
    }
    
    # check log version for meaningful mixtures
    weights <- seq( 1:mix$n_distributions )
    mix$weights <- weights / sum( weights )
    dd <- unlist( lapply( 1:mix$n_distributions, function( idx ) mix$distributions[[idx]]$d( 1.1 ) ) )
    expect_equal( mix$d( 1.1, log = T ), log( sum( dd * mix$weights ) ) )
    pd <- unlist( lapply( 1:mix$n_distributions, function( idx ) mix$distributions[[idx]]$p( 1.1 ) ) )
    expect_equal( mix$p( 1.1, log = T ), log( sum( pd * mix$weights ) ) )
    
    # check the distribution of random draws from mixture agree with CDF
    n_samples <- 1e4
    xs <- mix$r( n_samples )
    
    # get the quantiles of the sample
    probs <- seq( 0.1, 0.9, 0.1)
    rqs <- quantile( xs, probs )
    
    # compare to calculate CDF at these points (samples in a quantile are binomially distributed )
    ps <- mix$p( rqs )
    expect_lt( max( abs( ps - probs) / sqrt( probs * ( 1 - probs ) / n_samples ) ), 4 )
    
    # check within range of the quantule of these points
    qmin <- mix$q( probs - 4 * sqrt( probs * ( 1 - probs ) / n_samples ))
    qmax <- mix$q( probs + 4 * sqrt( probs * ( 1 - probs ) / n_samples ))
    expect_equal( sum( rqs < qmax ), length( probs ) )
    expect_equal( sum( rqs > qmin ), length( probs ) )
  }
} )


test_that( "Check error on invalid update of mixture distribution", {
  dists <- list(
    distribution.gamma( 2,1),
    distribution.gamma( 2,2) 
  )
  weights <- c( 0.5, 0.5)
  expect_no_error( mix <- distribution.mixture( dists, weights ) )
  nondist <- R6.class( "non-dist")
  expect_error( distribution.mixture( dists, 1 ) )
  expect_error( distribution.mixture( c( dists,nondist ), c( weights, 1 ) ) )
  expect_error( mix$weights <- 1 )
} )

