# Include R6_util_class.R and distribution_R6_class.R to guarantee base classes exist
# when loading the package prior to defining classes

################################################################################/
#  distribution.continuous.class
################################################################################/
#' Class: `distribution.continuous.class`
#' @description Base class for univariate continuous distributions
#'
#' @param support The support of the distribution, i.e. the subset of values for
#'   which the density is positive.
#' @param q          vector of quantiles.
#' @param p          vector of probabilities.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#' 
#' @field interfaces The list of available class interfaces.
#' @field support    The support of the continuous distribution, i.e. the subset
#'   of values for which the density is positive,
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
distribution.continuous.class <- R6.class(
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
    },
    ##############################################################################/
    # cumulative distribution function
    ##############################################################################/
    #' @description Evaluates the distribution function of a discrete random
    #'   variable with finite integer support given density function `$d()`
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      # Bounds of private$.support
      support_lower <- private$.support[ 1 ]
      support_upper <- private$.support[ 2 ]

      out <- sapply( q, function( q_ ){
        if ( is.na( q_ ) ){
          return( NA )
        } else if ( q_ <= support_lower ){
          return( 0 )
        } else if ( q_ > support_upper ){
          return( 1 )
        } else {
          return( integrate( self$d,
                             lower = support_lower, upper = q_,
                             log = FALSE )$value )
        }
      })
      
      if ( !lower.tail ) out <- 1 - out

      if ( log.p ){
        return( log( out ) )
      } else {
        return( out )
      }
    },
    ##############################################################################/
    # quantile function
    ##############################################################################/
    #' @description Evaluates the distribution function of a discrete random
    #'   variable with finite integer support given distribution function `$p()`
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      if ( log.p ) p <- exp( p )
      if ( !lower.tail ) p <- 1 - p
      if ( any( is.na( p ), p < 0, p > 1 ) )
        stop( "Values in `p` must be numeric values between 0 and 1.")
      if ( length( p ) == 0 ) return( numeric( 0 ) )
      
      # Bounds for first and last integer values x within private$.support
      support_lower <- private$.support[ 1 ]
      support_upper <- private$.support[ 2 ]
      
      transform <- if ( is.finite( support_lower ) ){
        if ( is.finite( support_upper ) ){
          # Mapping [0,1] -> [support_lower, support_upper]
          function( x ) support_lower + x * ( support_upper - support_lower )
        }
        else {
          # Mapping [0,1] -> [support_lower, Inf)
          function( x ) support_lower + x / ( 1 - x )
        }
      } else {
        if ( is.finite( support_upper ) ){
          # Mapping [0,1] -> (-Inf, support_upper]
          function( x ) support_upper - ( 1 - x ) / x
        } else {
          # Mapping [0,1] -> (-Inf, Inf)
          function( x ) tan( pi * ( x - 0.5 ) )
        }
      }
      
      density_p_shift <- function( x ){ self$p( transform( x ), log = FALSE ) - p }
      num_p         <- length( p )
      q <- uniroot.vectorized(
        f = density_p_shift,
        lower = rep( 0, num_p ),
        upper = rep( 1, num_p )
      )
      
      return( transform( q ) )
    }
  ),
  active = list(
    support = function( val ){
      private$.staticReturn( val, "support" )
    }
  )
)

################################################################################/
#  distribution.continuous.uniform
################################################################################/
#' Class: `distribution.continuous.uniform.class`
#' @description Derived class for an uniformly-distributed random variable on \code{[min, max]}
#'
#' @param min The lower bound of the uniform distribution
#' @param max The max bound of the uniform distribution
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
#' @field mean The mean of a uniform random variable on \code{[min, max]}.
#' @field sd The standard deviation of a uniform random variable on
#'   \code{[min, max]}.
#' @field var The variance of a uniform random variable on \code{[min,
#'   max]}.
distribution.continuous.uniform.class <- R6.class(
  classname = "distribution.continuous.uniform.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "Uniform",
    .param_names = c( "min", "max" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      if ( !( is.numeric( params$min ) && is.numeric( params$max ) ) )
        stop( "`min` and `max` must be numeric values.")
      if ( params$min > params$max )
        stop( "`min` must be strictly less than `max`.")
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.continuous.exponential.class`
    initialize = function( min = 0, max = 1 ){
      super$initialize( support = c( min, max ) )
      self$params <- list( min = min,
                           max = max )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a uniform random variable on
    #'   \code{[min, max]}.
    d = function( x, log = FALSE ){
      stats::dunif( x, min = private$.params$min, max = private$.params$max,
                    log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a uniform random variable on
    #'   \code{[min, max]}.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::punif( q, min = private$.params$min, max = private$.params$max,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a uniform random variable on
    #'   \code{[min, max]}.
    #'   rate `params$rate`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qunif( p, min = private$.params$min, max = private$.params$max,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a uniform random variable on
    #'   \code{[min, max]}.
    r = function( n ){
      stats::runif( n, min = private$.params$min, max = private$.params$max )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( ( private$.params$lower + private$.params$max ) / 2 )
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
      return( ( private$.params$max - private$.params$lower )^2 / 12 )
    }
  )
)

#' distribution.uniform
#' 
#' Constructor function for an object of class `distribution.continuous.uniform.class`
#' 
#' @param min The lower bound of the uniform distribution
#' @param max The max bound of the uniform distribution
#' 
#' @returns An object of class [[distribution.continuous.uniform.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.uniform <- function( min = 0, max = 1 ){
  distribution.continuous.uniform.class$new( min = min,
                                             max = max )
}

################################################################################/
#  distribution.continuous.exponential
################################################################################/
#' Class: `distribution.continuous.exponential.class`
#' @description Derived class for an exponentially-distributed random variable.
#'
#' @param rate The rate of the exponential distribution
#' @param offset The amount by which the exponential distribution is shifted.
#'   Creates a random variable `X~offset`+Exp(`params$rate`)
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
#' @field offset The amount by which the exponential distribution is shifted.
#'   Creates a random variable `X~offset`+Exp(`params$rate`)

distribution.continuous.exponential.class <- R6.class(
  classname = "distribution.continuous.exponential.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "Exponential",
    .param_names = c( "rate" ),
    .offset       = 0,
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
    initialize = function( rate = 1, offset = 0 ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( rate   = rate )
      self$offset <- offset
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for an exponential random variable with
    #'   rate `params$rate`.
    d = function( x, log = FALSE ){
      stats::dexp( x - self$offset,
                   rate = private$.params$rate, log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for an exponential random
    #'   variable with rate `params$rate`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pexp( q - self$offset, rate = private$.params$rate,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for an exponential random variable with
    #'   rate `params$rate`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      self$offset + stats::qexp( p, rate = private$.params$rate,
                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for an exponential random variable
    #'   with rate `params$rate`.
    r = function( n ){
      self$offset + stats::rexp( n, rate = private$.params$rate )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( self$offset + 1 / private$.params$rate )
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
    },
    ############################################################################/
    # offset
    ############################################################################/
    offset = function( new_val ){
      if ( missing( new_val ) ) return( private$.offset )
      if ( !is.numeric( new_val ) )
        stop( "`$offset` must be a numeric value" )
      private$.offset <- new_val
      private$.support <- c( new_val, Inf )
    }
  )
)

#' distribution.exponential
#' 
#' Constructor function for an object of class `distribution.continuous.exponential.class`
#' 
#' @param rate vector of rates
#' @param offset The amount by which the exponential distribution is shifted.
#'   Creates a random variable `X~offset`+Exp(`params$rate`)
#' 
#' @returns An object of class [[distribution.continuous.exponential.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.exponential <- function( rate = 1, offset = 0 ){
  distribution.continuous.exponential.class$new( rate = rate, offset = offset )
}

################################################################################/
#  distribution.continuous.gamma
################################################################################/
#' Class: `distribution.continuous.gamma.class`
#' @description Derived class for a gamma-distributed random variable.
#'
#' @param shape The shape of the gamma distribution
#' @param rate  The rate of the gamma distribution
#' @param scale an alternative way to specify the rate
#' @param offset offset The amount by which the gamma distribution is shifted.
#'   Creates a random variable `X~offset`+Gamma(`params$shape`, `params$rate`)
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
#' @field offset The amount by which the gamma distribution is shifted.
#'   Creates a random variable `X~offset`+Gamma(`params$shape`, `params$rate`)

distribution.continuous.gamma.class <- R6.class(
  classname = "distribution.continuous.gamma.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name         = "gamma",
    .param_names  = c( "shape", "rate", "scale" ),
    .offset       = 0,
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
    initialize = function( shape, rate, scale, offset = 0 ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( shape = shape,
                           rate  = rate,
                           scale = scale )
      self$offset <- offset
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a gamma random variable with
    #'   rate `params$rate`.
    d = function( x, log = FALSE ){
      stats::dgamma( x - self$offset, shape = private$.params$shape, rate = private$.params$rate,
                     log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a gamma random
    #'   variable with rate `params$rate`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pgamma( q - self$offset, shape = private$.params$shape, rate = private$.params$rate,
                     lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a gamma random variable with
    #'   rate `params$rate`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      self$offset + stats::qgamma( p, shape = private$.params$shape, rate = private$.params$rate,
                                   lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a gamma random variable
    #'   with rate `params$rate`.
    r = function( n ){
      self$offset + stats::rgamma( n, shape = private$.params$shape, rate = private$.params$rate )
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
      return( self$offset + private$.params$shape / private$.params$rate )
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
    },
    ############################################################################/
    # offset
    ############################################################################/
    offset = function( new_val ){
      if ( missing( new_val ) ) return( private$.offset )
      if ( !is.numeric( new_val ) )
        stop( "`$offset` must be a numeric value" )
      private$.offset <- new_val
      private$.support <- c( new_val, Inf )
    }
  )
)

#' distribution.gamma
#' 
#' Constructor function for an object of class `distribution.continuous.gamma.class`
#' 
#' @param shape The shape of the gamma distribution
#' @param rate  The rate of the gamma distribution
#' @param scale an alternative way to specify the rate
#' @param offset offset The amount by which the gamma distribution is shifted.
#'   Creates a random variable `X~offset`+Gamma(`params$shape`, `params$rate`)
#' 
#' @returns An object of class [[distribution.continuous.gamma.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.gamma <- function( shape, rate, scale, offset = 0 ){
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
  
  distribution.continuous.gamma.class$new( shape  = shape,
                                           rate   = rate,
                                           scale  = scale,
                                           offset = offset )
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
distribution.continuous.normal.class <- R6.class(
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
#' @seealso [Mastiff-Distributions]
#' @export
distribution.normal <- function( mean, sd ){
  distribution.continuous.normal.class$new( mean = mean,
                                            sd   = sd )
}

################################################################################/
#  distribution.continuous.lognormal
################################################################################/
#' Class: `distribution.continuous.lognormal.class`
#' @description Derived class for a lognormally-distributed random variable.
#'
#' @param meanlog    the mean of log(X)
#' @param sdlog      the standard deviation of log(X)
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
#' @field mean      the mean of the distribution
#' @field sd        the standard deviation of the distribution
#' @field var       the variance of the distribution
distribution.continuous.lognormal.class <- R6.class(
  classname = "distribution.continuous.lognormal.class",
  inherit   = distribution.continuous.class,
  private   = list(
    .name    = "lognormal",
    .param_names = c( "meanlog",  "sdlog" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      if ( !is.numeric( params$meanlog ) )
        stop( "`params$meanlog` must be a numeric value.")
      if ( params$sdlog < 0 || !is.numeric( params$sdlog ) )
        stop( "`params$sdlog` must be a non-negative numeric value.")
      
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class
    #'   `distribution.continuous.normal.class`
    initialize = function( meanlog, sdlog ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( meanlog = meanlog,
                           sdlog   = sdlog )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a lognormal random variable with mean log(X)
    #'   `$params$meanlog` and standard deviation log(X) `$params$sdlog`.
    d = function( x, log = FALSE ){
      stats::dlnorm( x, meanlog = private$.params$meanlog, sdlog = private$.params$sdlog,
                    log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a lognormal random variable
    #'   with mean log(X) `$params$meanlog` and standard deviation log(X) `$params$sdlog`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::plnorm( q, meanlog = private$.params$meanlog, sdlog = private$.params$sdlog,
                    lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a lognormal random variable with mean
    #'  log(X)  `$params$meanlog` and standard deviation log(X) `$params$sdlog`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qlnorm( p, meanlog = private$.params$meanlog, sdlog = private$.params$sdlog,
                    lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a lognormal random variable with
    #'   mean log(X) `$params$meanlog` and standard deviation logx(X) `$params$sdlog` .
    r = function( n ){
      stats::rlnorm( n, meanlog = private$.params$meanlog, sdlog = private$.params$sdlog )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( exp( private$.params$meanlog + 0.5 * private$.params$sdlog^2 ) )
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
      sigma2 <- private$.params$sdlog^2
      mu     <- private$.params$meanlog
      return( ( exp( sigma2) - 1 ) * exp( 2 * mu + sigma2 ) )
    }
  )
)

#' distribution.lognormal
#' 
#' Constructor function for an object of class `distribution.continuous.lognormal.class`
#' 
#' @param meanlog The mean of log(X)
#' @param sdlog   The standard deviation log(X)
#' 
#' @returns An object of class [[distribution.continuous.lognormal.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.lognormal <- function( meanlog, sdlog ){
  distribution.continuous.lognormal.class$new( meanlog = meanlog,
                                               sdlog   = sdlog )
}