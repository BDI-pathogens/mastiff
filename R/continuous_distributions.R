##################################################################/
#  distribution.continuous.class
###################################################################/
distribution.continuous.class <- utils.class(
  classname = "distribution.continuous.class",
  inherit   = distribution.abstract.class,
  private   = list(
    .support = c( -Inf, Inf )
  ),
  public = list(
    initialize = function( support = c( -Inf, Inf ) ){
      private$.support <- support
    }
  ),
  active = list(
    support = function( val ){
      private$.staticReturn( val, "support" )
    }
  )
)

##################################################################/
#  distribution.continuous.exponential
###################################################################/
distribution.continuous.exponential.class <- utils.class(
  classname = "distribution.continuous.exponential.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "Exponential",
    .param_names = c( "rate" ),
    .check_params = function( params ){
      if ( params$rate < 0 ){
        stop( "`params$rate` must be >0.")
      }
      return( NULL )
    }
  ),
  public = list(
    ##############################################################################/
    # initialize
    ##############################################################################/
    #' @description Create a new object of class `distribution.continuous.class`
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
  active = list()
)

#' distribution.exponential
#' 
#' Constructor function for an object of class `distribution.continuous.exponential.class`

distribution.exponential <- function( rate = 1 ){
  distribution.continuous.exponential.class$new( rate = rate )
}