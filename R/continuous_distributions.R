# Include R6_util_class.R and R6_distribution.R to guarantee base classes exist
# when loading the package prior to defining classes

################################################################################/
#  distribution.continuous.class
################################################################################/
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
    ############################################################################/
    # initialize
    ############################################################################/
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
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.continuous.exponential.class`
    initialize = function( rate = 1 ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( rate = rate )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for an exponential random variable with
    #'   rate `params$rate`.
    d = function( x, log = FALSE ){
      stats::dexp( x, rate = private$.params$rate, log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for an exponential random
    #'   variable with rate `params$rate`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pexp( q, rate = private$.params$rate,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for an exponential random variable with
    #'   rate `params$rate`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qexp( p, rate = private$.params$rate,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for an exponential random variable
    #'   with rate `params$rate`.
    r = function( n ){
      stats::rexp( n, rate = private$.params$rate )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( 1 / private$.params$rate )
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( 1 / private$.params$rate )
    },
    ############################################################################/
    # variance
    ############################################################################/
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

################################################################################/
#  distribution.continuous.gamma
################################################################################/
#' Class: `distribution.continuous.gamma.class`
#' @description Derived class for a gamma-distributed random variable.
#'
#' @param shape The shape of the gamma distribution
#' @param rate The rate of the gamma distribution
#' @param scale an alternative way to specify the rate
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
#' @field params       Named list of distribution parameters
#' @field mean The mean of a gamma distribution with shape `$params$shape` and
#'   rate `$params$rate`.
#' @field sd The standard deviation of a gamma distribution with shape
#'   `$params$shape` and rate `$params$rate`.
#' @field var The variance of a gamma distribution with shape `$params$shape`
#'   and rate `$params$rate`.

distribution.continuous.gamma.class <- utils.class(
  classname = "distribution.continuous.gamma.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "gamma",
    .param_names = c( "shape", "rate", "scale" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      if ( params$shape < 0 || !is.numeric( params$shape ) )
        stop( "`params$shape` must be a non-negative numeric value.")
      if ( params$rate < 0 || !is.numeric( params$rate ) )
        stop( "`params$rate` must be a non-negative numeric value.")
      if ( params$scale < 0 || !is.numeric( params$scale ) )
        stop( "`params$scale` must be a non-negative numeric value.")
      
      # When updating params by name, need to maintain rate = 1 / scale if only
      # one of rate or scale is updated
      if ( !is.null( self$params$rate ) ){
        if ( ( params$rate != self$params$rate ) &&
             ( params$scale == self$params$scale ) )
          params$scale <- 1 / params$rate
        
        if ( ( params$rate == self$params$rate ) &&
             ( params$scale != self$params$scale ) )
          params$rate <- 1 / params$scale
      }
      
      if ( abs( params$scale * params$rate - 1 ) > 1e-10 )
        stop( "`params$rate` and `params$scale` are inconsistent. Must have `params$rate = 1 / params$scale`." )
      
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.continuous.gamma.class`
    initialize = function( shape, rate, scale ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( shape = shape,
                           rate  = rate,
                           scale = scale )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a gamma random variable with
    #'   rate `params$rate`.
    d = function( x, log = FALSE ){
      stats::dgamma( x, shape = private$.params$shape, rate = private$.params$rate,
                     log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a gamma random
    #'   variable with rate `params$rate`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pgamma( q, shape = private$.params$shape, rate = private$.params$rate,
                     lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a gamma random variable with
    #'   rate `params$rate`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qgamma( p, shape = private$.params$shape, rate = private$.params$rate,
                     lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a gamma random variable
    #'   with rate `params$rate`.
    r = function( n ){
      stats::rgamma( n, shape = private$.params$shape, rate = private$.params$rate )
    }
  ),
  active = list(
    ############################################################################/
    # params
    ############################################################################/
    # Allow either rate or shape to be input and fill any missing parameters
    # given input values before dispatching to parent class
    params = function( new_val ){
      if ( missing( new_val ) ) return( private$.params )
      
      if ( is.null( new_val$rate ) ){
        if ( is.null( new_val$scale ) ){
          stop( "At least one of `params$rate` and `params$scale` must be set." )
        }
        new_val$rate <- 1 / new_val$scale
      }
      
      if ( is.null( new_val$scale ) ){
        # Implicit !is.null( new_val$rate )
        new_val$scale <- 1 / new_val$rate
      }
      
      super$params <- new_val
    },
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.params$shape / private$.params$rate )
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( sqrt( self$var ) )
    },
    ############################################################################/
    # variance
    ############################################################################/
    var = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$var`" )
      return( self$mean / private$.params$rate )
    }
  )
)

#' distribution.gamma
#' 
#' Constructor function for an object of class `distribution.continuous.gamma.class`
#' 
#' @param rate vector of rates
#' 
#' @returns An object of class [[distribution.continuous.gamma.class]]
#'
#' @export
distribution.gamma <- function( shape, rate, scale ){
  if ( missing( rate ) ){
    if ( missing( scale ) ){
      stop( "At least one of `rate` and `shape` must be set." )
    } else {
      rate <- 1 / scale
    }
  }
  
  if ( missing( scale ) ){
    # Implicit !missing( rate )
    scale <- 1 / rate
  }
  
  distribution.continuous.gamma.class$new( shape = shape,
                                           rate  = rate,
                                           scale = scale )
}

################################################################################/
#  distribution.continuous.normal
################################################################################/
#' Class: `distribution.continuous.normal.class`
#' @description Derived class for a normally-distributed random variable.
#'
#' @param mean The mean of the normal distribution.
#' @param sd The standard deviation of the normal distribution.
#' @param x          vector of quantiles.
#' @param q          vector of quantiles.
#' @param p          vector of probabilities.
#' @param n          number of observations. If `length( n ) > 1`, the length is
#'   taken to be the number required.
#' @param log        logical; if TRUE, probabilities p are given as `log(p)`.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are 
#'   \eqn{P[ X \leq x ]}, otherwise, \eqn{P[X>x]}.
#' 
#' @field interfaces The list of available class interfaces
#' @field mean The mean of a normal distribution with rate `$params$rate`.
#' @field sd The standard deviation of a normal distribution with rate
#'   `$params$rate`.
#' @field var The variance of a normal distribution with rate
#'   `$params$rate`.
distribution.continuous.normal.class <- utils.class(
  classname = "distribution.continuous.normal.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "normal",
    .param_names = c( "mean",
                      "sd" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      if ( !is.numeric( params$mean ) )
        stop( "`params$mean` must be a numeric value.")
      if ( params$sd < 0 || !is.numeric( params$sd ) )
        stop( "`params$sd` must be a non-negative numeric value.")

      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class
    #'   `distribution.continuous.normal.class`
    initialize = function( mean, sd ){
      super$initialize( support = c( -Inf, Inf ) )
      self$params <- list( mean = mean,
                           sd   = sd )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a normal random variable with mean
    #'   `$params$mean` and standard deviation `$params$sd`.
    d = function( x, log = FALSE ){
      stats::dnorm( x, mean = private$.params$mean, sd = private$.params$sd,
                    log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a normal random variable
    #'   with mean `$params$mean` and standard deviation `$params$sd`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pnorm( q, mean = private$.params$mean, sd = private$.params$sd,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a normal random variable with mean
    #'   `$params$mean` and standard deviation `$params$sd`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qnorm( p, mean = private$.params$mean, sd = private$.params$sd,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a normal random variable with
    #'   mean `$params$mean` and standard deviation `$params$sd`.
    r = function( n ){
      stats::rnorm( n, mean = private$.params$mean, sd = private$.params$sd )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.params$mean )
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( private$.params$sd )
    },
    ############################################################################/
    # variance
    ############################################################################/
    var = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$var`" )
      return( self$sd^2 )
    }
  )
)

#' distribution.normal
#' 
#' Constructor function for an object of class `distribution.continuous.normal.class`
#' 
#' @param mean The mean of the normal distribution
#' @param sd   The standard deviation of the normal distribution
#' 
#' @returns An object of class [[distribution.continuous.normal.class]]
#'
#' @export
distribution.normal <- function( mean, sd ){
  distribution.continuous.normal.class$new( mean = mean,
                                            sd   = sd )
}