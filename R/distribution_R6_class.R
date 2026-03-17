# Include R6_util_class.R to guarantee R6.class() and R6.class.interface()
# exist when loading the package prior to defining classes

#' @name Mastiff-Distributions
#' @title Distribution Classes
#' 
#' @description Distributions implemented as R6 classes in [mastiff] 
#' @format NULL
#' @usage NULL
#' 
#' @details
#' [mastiff] introduces a R6 class structure which is used to combine the
#' density, distribution function, quantile function and generation of random
#' deviates into a single object.
#' 
#' Parameters for a distribution are set at initialisation and can be updated
#' via a named list `$params` stored in the distribution class.
#' 
#' Each distribution includes methods
#' \tabular{lll}{
#' `$d(x)` \tab  \tab Evaluates the density at values `x` \cr
#' `$p(q)` \tab  \tab Evaluates the distribution function at values `q` \cr
#' `$q(p)` \tab  \tab Evaluates the quantile function at values `p` \cr
#' `$r(n)` \tab  \tab Generates `n` random values from the distribution \cr
#' `$mean` \tab  \tab Returns the mean of the distribution \cr
#' `$sd`   \tab  \tab Returns the standard deviation of the distribution \cr
#' `$var`  \tab  \tab Returns the variance of the distribution \cr
#' }
#' 
#' @examples
#' # Construct a Poisson( 1 ) random variable
#' Pois_RV <- distribution.poisson( lambda = 1 )
#' 
#' # Evaluate the density, equivalent to dpois( 0 : 5, lambda = 1 )
#' Pois_RV$d( 0 : 5 )
#' 
#' # Evaluate the distribution function, equivalent to ppois( 0 : 5, lambda = 1 )
#' Pois_RV$p( 0 : 5 )
#' 
#' # Evaluate the quantile function, equivalent to qpois( c( 0.5, 0.8 ), lambda = 1 )
#' Pois_RV$q( c( 0.5, 0.8 ) )
#' 
#' # Generate random deviates, equivalent to rpois( 10, lambda = 1 )
#' Pois_RV$r( 10 )
#' 
#' # Update parameters to a Poisson( 10 ) random variable
#' Pois_RV$params <- list( lambda = 10 )
#' mean( Pois_RV$r( 1e5 ) )
#' 
#' @section Discrete Distributions:
#'
#' Discrete distributions with class [distribution.discrete.class]
#' \tabular{ll}{
#' [distribution.binomial] \tab Binomial distribution with size `size` and success probability `prob` \cr
#' [distribution.negative_binomial] \tab Negative Binomial distribution with size `size` and success probability `prob` \cr
#' [distribution.point_mass] \tab Point mass at `value` \cr
#' [distribution.poisson]  \tab Poisson distribution with mean `lambda` \cr
#' }
#' 
#' @section Continuous Distributions:
#' 
#' Discrete distributions with class [distribution.continuous.class]
#' \tabular{ll}{
#' [distribution.exponential] \tab Exponential distribution with rate `rate` \cr
#' [distribution.gamma] \tab Gamma distribution with shape `shape` and rate `rate` \cr
#' [distribution.normal]  \tab Normal distribution with mean `mean` and standard deviation `sd` \cr
#' [distribution.uniform] \tab Uniform distribution on `[min, max]` \cr
#' }
#' 
#' @include R6_class.R
NULL

##################################################################/
#  distribution.interface
###################################################################/
# Interface for all distributions, enforcing the definition of
#   - d: density function
#   - p: distribution function
#   - q: quantile function
#   - r: random deviates
distribution.interface <- R6.interface(
  interfacename = "distribution.interface",
  public = list(
    ############################################################################
    # density
    ############################################################################
    d = function( x, log = FALSE ){
      stop( "`d` not implemented on derived class" )
    },
    ############################################################################
    # distribution function
    ############################################################################
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stop( "`p` not implemented on derived class" )
    },
    ############################################################################
    # quantile function
    ############################################################################
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stop( "`q` not implemented on derived class" )
    },
    ############################################################################
    # random deviates
    ############################################################################
    r = function( n ){
      stop( "`r` not implemented on derived class" )
    }
  )
)

##################################################################/
#  distribution.abstract.class
###################################################################/
#' Class: `distribution.abstract.class`
#' @description Base class for derived distributions
#'
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
#' 
#' @field name         The name of the distribution
#' @field param_names  The names of all distribution parameters
#' @field params       Named list of distribution parameters
#' @field interfaces   The list of available class interfaces
#' @field mean         Mean of the distribution
#' @field sd           Standard deviation of the distribution
#' @field var          Variance of the distribution

distribution.abstract.class <- R6.class(
  classname  = "distribution.abstract.class",
  interfaces = list( distribution.interface ),
  private    = list(
    .name   = NULL, # Distribution name
    .params = list(), # Named list of distribution parameters
    .param_names = character(), # Character vector of names for params list
    .check_params = function( params ){
      # Generic .check_params() verifies that params stores only elements listed
      # in param_names.
      #   - Should be updated on all derived classes to check input 
      #     parameter values are also plausible.
      if ( ( !is.list( params ) ) || length( names( params ) ) == 0 ){
        stop( sprintf( "`$params` must be a named list with elements `%s`",
                       paste0( private$.param_names, collapse = "`, " ) ) )
      }
      
      input_names <- names( params )
      if ( !all( input_names %in% private$.param_names,
                 private$.param_names %in% input_names ) )
        stop( sprintf( "`$params` must be a named list with elements `%s`. Input list contained elements `%s`",
                       paste( private$.param_names, collapse = '`, ' ),
                       paste( input_names, collapse = '`, ' ) ) )
      
      return( NULL )
    },
    .staticReturn = function( val, name ){
      # Creates static active binding `name` with value `val` which cannot be
      # updated after class creation
      if( !missing( val ) )
        stop( sprintf( "cannot set %s", name ) )
      
      privateName <- sprintf( ".%s", name )
      return( private[[ privateName ]])
    },
    .updateReturn = function( val, type, param = NULL ){
      privateType <- sprintf( ".%s", type )
      
      if( is.null( param ) ) {
        if( !is.null( val ) )
          private[[ privateType ]] <- val
        return( private[[ privateType ]] )
      }
      
      if( !is.null( val ) ) {
        all <- private[[ privateType ]]
        all[[ param ]] <- val
        private[[ privateType ]] <- all
      }
      return( private[[ privateType ]][[ param ]] )
    }
  ),
  active = list(
    name         = function( val ) private$.staticReturn( val, "name" ),
    param_names  = function( val ) private$.staticReturn( val, "param_names" ),
    params = function( new_val ){
      if ( missing( new_val ) ) return( private$.params )
      private$.check_params( new_val )
      private$.params <- new_val
    },
    mean = function( val ) stop( "`mean` not implemented on derived class" ),
    sd   = function( val ) stop( "`sd` not implemented on derived class" ),
    var  = function( val ) stop( "`var` not implemented on derived class" )
  ),
  public = list(
    ##############################################################################/
    # initialize
    ##############################################################################/
    #' @description Create a new object of class `distribution.abstract.class`
    initialize = function()
      stop( "Object of class `distribution.abstract.class`"),
    ##############################################################################/
    # density
    ##############################################################################/
    #' @description Template base class function for density function of a
    #'   distribution
    d = function( x, log = FALSE )
      stop( "`d` not implemented on derived class"),
    ##############################################################################/
    # cumulative distribution function
    ##############################################################################/
    #' @description Template base class function for cumulative distribution
    #'   function
    p = function( q, lower.tail = TRUE, log.p = FALSE )
      stop( "`p` not implemented on derived class"),
    ##############################################################################/
    # quantile function
    ##############################################################################/
    #' @description Template base class function for quantile function of a
    #'   distribution
    q = function( p, lower.tail = TRUE, log.p = FALSE )
      stop( "`q` not implemented on derived class"),
    ##############################################################################/
    # random deviates
    ##############################################################################/
    #' @description Template base class function for sampling random variates
    r = function( n )
      stop( "`r` not implemented on derived class")
  )
)

################################################################################/
# is.distribution
################################################################################/
#' @title Distribution Classes
#'
#' @description Available distributions implemented in `mastiff`.
#'
#' @param x An R object.
#' 
#' `is.distribution( x )` checks where an object `x` inherits from either
#' [distribution.discrete.class] or [distribution.continuous.class]
#' 
#' `mastiff`
is.distribution <- function( x ) inherits( x, 'distribution.abstract.class' )