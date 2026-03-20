# Include R6_util_class.R and distribution_R6_class.R to guarantee base classes exist
# when loading the package prior to defining classes

################################################################################/
#  distribution.mixture.class
################################################################################/
#' Class: `distribution.mixture.class`
#' @description Class to describe the mixture of distributions
#'
#' @param distributions list of distributions in the mixture
#' @param weights       vector of weights of the distributions in the mixture
#' @param x          vector of quantiles.
#' @param q          vector of quantiles.
#' @param p          vector of probabilities.
#' @param n          number of observations. If `length( n ) > 1`, the length is
#'   taken to be the number required.
#' @param log        logical; if TRUE, probabilities p are given as `log(p)`.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#'   
#' @field n_distributions the number of distributions in the mixture
#' @field distributions   the distributions in the mixture
#' @field weights         the weights of the distributions in the mixture
#' @field interfaces      the list of available class interfaces
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
distribution.mixture.class <- R6.class(
  classname = "distribution.mixture.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .distributions = NULL,
    .n_distributions = NULL,
    .weights = NULL,
    .logExpAdd = function( ds, w ) {
      eps = 1e-16
      nw  = ncol( ds )
      ns  = nrow( ds ) 
      ds  <- t( ds ) + matrix( log( w + eps ), nrow = nw, ncol = ns ) 
      max <- matrixStats::colMaxs( ds )
      ds  <- ds - matrix( rep( max, each = nw ), nrow = nw, ncol = ns )  
      return( log( colSums( exp( ds ) ) ) + max )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.mixture.class`
    initialize = function( distributions, weights ){
      stopifnot( length( distributions) == length( weights ) )
      stopifnot( all( unlist( lapply( distributions, function( d ) is.distribution( d ) ) ) ) )
      stopifnot( all( unlist( lapply( distributions, function( d ) inherits( d, "distribution.continuous.class" ) ) ) ) )
      stopifnot( abs( sum( weights ) - 1 ) < 1e-10 )
      
      private$.distributions   <- distributions 
      private$.weights         <- weights / sum( weights )
      private$.n_distributions <- length( distributions )
      private$.support         <- c( min( unlist( lapply( distributions, function( d ) d$support[1] ))),
                                     max( unlist( lapply( distributions, function( d ) d$support[2] ))))
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a random variable of the mixture
    d = function( x, log = FALSE ){
      ds <- matrix( unlist( lapply( private$.distributions, 
        function( d ) d$d( x, log = log ) ) ), 
        ncol = self$n_distributions )  
      
      if( log ) {
        return( private$.logExpAdd( ds, self$weights ) )
      } else {
        return( ( ds %*% self$weights )[, 1 ] )
      }
    },
    ##############################################################################/
    # cumulative distribution function
    ##############################################################################/
    #' @description Evaluates the distribution function of the mixture
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      ps <- matrix( unlist( lapply( private$.distributions, 
        function( d ) d$p( q, lower.tail = lower.tail, log.p = log.p ) ) ), 
        ncol = self$n_distributions ) 
     
      if( log.p ) {
        return( private$.logExpAdd( ps, self$weights ) )
      } else {
        return( ( ps %*% self$weights )[, 1 ] )
      }
    },
    ##############################################################################/
    # quantile function
    ##############################################################################/
    #' @description Evaluates the quantile function of the mixture
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      super$q( p, lower.tail = lower.tail, log.p = log.p )      
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random samples of the mixture
    r = function( n ){
      dists  <- self$distributions
      n_dist <- length( dists )
      model  <- sample( 1:n_dist, n, replace = TRUE, prob = self$weights )
    
      ret <- vector( mode = "numeric", length = n_dist )
      for( ddx in 1:n_dist ) {
        idxs <- which( model == ddx )
        if( length( idxs) > 0 ) 
          ret[ idxs ] <- dists[[ddx]]$r( length( idxs ) )
      }
      return( ret )
    }
  ),
  active = list(
    ############################################################################/
    # n_distributions
    ############################################################################/
    n_distributions = function( val ){
      private$.staticReturn( val, "n_distributions" )
    },
    ############################################################################/
    # distributions
    ############################################################################/
    distributions = function( val ){
      if( missing( val ) )
        return( private$.staticReturn( val, "distributions" ) )
      
      # allow updates of values on distributions objects
      old_val <- private$.distributions 
      stopifnot( length( val ) == length( old_val ) )
      for( idx in 1:length( val ) ) {
        stopifnot( is.distribution( val[[ idx ]] ) ) 
        stopifnot( data.table::address( val[[ idx ]] ) == 
                   data.table::address( old_val[[ idx ]] ) )
      }
      
    },
    ############################################################################/
    # weights
    ############################################################################/
    weights = function( new = NA ){
      if( length( new) == 1 ) 
        if( is.na( new ) ) {
          return( private$.weights )  
        }
      
      stopifnot( length( new ) == self$n_distributions )
      stopifnot( abs( 1 - sum( new ) ) < 1e-10 )
      private$.weights <- new / sum( new )
    }
  )
)

#' distribution.mixture
#' 
#' Constructor function for an object of class `distribution.mixture.class`
#' 
#' @param distributions a list of distributions (all continuous)
#' @param weights the mixture weight of each distribution (sum to 1)
#' 
#' @returns An object of class [[distribution.mixture.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.mixture <- function( distributions, weights ){
  distribution.mixture.class$new( distributions, weights )
}



