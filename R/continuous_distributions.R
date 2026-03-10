# Include R6_util_class.R and R6_distribution.R to guarantee base classes exist
# when loading the package prior to defining classes

##################################################################/
#  distribution.continuous.class
###################################################################/
#' Class: `distribution.continuous.class`
#' @description Base class for univariate continuous distributions
#'
#' @param support The support of the distribution, i.e. the subset of values for
#'   which the density is positive.
#' 
#' 
#' @field interfaces The list of available class interfaces.
#' @field support    The support of the continuous distribution, i.e. the subset
#'   of values for which the density is positive,
#' 
#' @include R6_util_class.R R6_distribution.R
distribution.continuous.class <- utils.class(
  classname = "distribution.continuous.class",
  inherit   = distribution.abstract.class,
  private   = list(
    .support = c( -Inf, Inf )
  ),
  public = list(
    ##############################################################################/
    # initialize
    ##############################################################################/
    #' @description Create a new object of class `distribution.continuous.class`
    initialize = function( support = c( -Inf, Inf ) ){
      private$.support <- support
    }
    
    ### TODO: Add generic p, q and r functions for continuous base class
  ),
  active = list(
    support = function( val ){
      private$.staticReturn( val, "support" )
    }
  )
)

################################################################################/
#  distribution.continuous.exponential
################################################################################/
#' Class: `distribution.continuous.exponential.class`
#' @description Derived class for an exponentially-distributed random variable.
#'
#' @param rate The rate of the exponential distribution
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
#' @field interfaces The list of available class interfaces
#' @field mean The mean of an exponential distribution with rate `$params$rate`.
#' @field sd The standard deviation of an exponential distribution with rate
#'   `$params$rate`.
#' @field var The variance of an exponential distribution with rate
#'   `$params$rate`.
distribution.continuous.exponential.class <- utils.class(
  classname = "distribution.continuous.exponential.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "Exponential",
    .param_names = c( "rate" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      # Check that params$rate is a non-negative numeric value
      if ( !is.numeric( params$rate ) )
        stop( "`params$rate` must be a numeric value.")
      if ( params$rate < 0 )
        stop( "`params$rate` must be >0.")
      return( NULL )
    }
  ),
  public = list(
    ##############################################################################/
    # initialize
    ##############################################################################/
    #' @description Create a new object of class `distribution.continuous.exponential.class`
    initialize = function( rate = 1 ){
      super$initialize( support = c( 0, Inf ) )
      private$.params <- list( rate = rate )
    },
    ##############################################################################/
    # density
    ##############################################################################/
    #' @description Density function for an exponential random variable with
    #'   rate `params$rate`.
    d = function( x, log = FALSE ){
      stats::dexp( x, rate = private$.params$rate, log = log )
    },
    ##############################################################################/
    # distribution function
    ##############################################################################/
    #' @description Cumulative density function for an exponential random
    #'   variable with rate `params$rate`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pexp( q, rate = private$.params$rate,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ##############################################################################/
    # quantile function
    ##############################################################################/
    #' @description Quantile function for an exponential random variable with
    #'   rate `params$rate`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qexp( p, rate = private$.params$rate,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ##############################################################################/
    # random deviates
    ##############################################################################/
    #' @description Generates random deviates for an exponential random variable
    #'   with rate `params$rate`.
    r = function( n ){
      stats::rexp( n, rate = private$.params$rate )
    }
  ),
  active = list(
    ##############################################################################/
    # mean
    ##############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( 1 / private$.params$rate )
    },
    ##############################################################################/
    # standard deviation
    ##############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( 1 / private$.params$rate )
    },
    ##############################################################################/
    # variance
    ##############################################################################/
    var = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$var`" )
      return( 1 / private$.params$rate^2 )
    }
  )
)

#' distribution.exponential
#' 
#' Constructor function for an object of class `distribution.continuous.exponential.class`
#' 
#' @param rate vector of rates
#' 
#' @returns An object of class [[distribution.continuous.exponential.class]]
#'
#' @export
distribution.exponential <- function( rate = 1 ){
  distribution.continuous.exponential.class$new( rate = rate )
}