# Include utils_R6.R and distribution_R6_class.R to guarantee base classes exist
# when loading the package prior to defining classes

################################################################################/
#  distribution.discrete.class
################################################################################/
#' Class: `distribution.discrete.class`
#' @description Base class for univariate discrete distributions
#'
#' @param support The support of the distribution, i.e. the subset of integers
#'   for which the density is positive.
#' @param q          vector of quantiles.
#' @param p          vector of probabilities.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#' 
#' @field interfaces The list of available class interfaces.
#' @field support    The support of the distribution, i.e. the subset of values
#'   for which the density is positive,
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
distribution.discrete.class <- 
R6.class(
  classname = "distribution.discrete.class",
  inherit   = distribution.abstract.class,
  private   = list(
    .support = c( 0, Inf )
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.discrete.class`
    initialize = function( support = c( 0, Inf ) ){
      private$.support <- support
    },
    ##############################################################################/
    # cumulative distribution function
    ##############################################################################/
    #' @description Evaluates the distribution function of a discrete random
    #'   variable with finite integer support given density function `$d()`
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      if ( any( !is.finite( private$.support ) ) ){
        stop( "`p` not implemented for discrete distributions with infinite support" )
      }
      q <- floor( q )
      
      # Bounds for first and last integer values x within private$.support
      support_lower <- ceiling( private$.support[ 1 ] )
      support_upper <- floor( private$.support[ 2 ] )
      
      q_max <- max( q, na.rm = TRUE )
      if ( q_max >= support_lower ){
        x <- support_lower : min( support_upper, q_max )
        pdf <- self$d( x, log = FALSE )
        cdf <- cumsum( pdf )
        
        out <- sapply( q, function( q_ ){
          if ( is.na( q_ ) ){
            return( NA )
          } else if ( q_ < support_lower ){
            return( 0 )
          } else if ( q_ > support_upper ){
            return( 1 )
          } else {
            idx <- 1 + q_ - support_lower
            return( cdf[ idx ] )
          }
        })
      } else {
        out <- rep( 0, length( q ) )
      }
      
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
      if ( any( !is.finite( private$.support ) ) ){
        stop( "`q` not implemented for discrete distributions with infinite support" )
      }
      if ( log.p ) p <- exp( p )
      if ( !lower.tail ) p <- 1 - p
      if ( any( is.na( p ), p < 0, p > 1 ) )
        stop( "Values in `p` must be numeric values between 0 and 1.")
      if ( length( p ) == 0 ) return( numeric( 0 ) )
      # Bounds for first and last integer values x within private$.support
      support_lower <- ceiling( private$.support[ 1 ] )
      support_upper <- floor( private$.support[ 2 ] )
      x <- seq.int( support_lower, support_upper )

      lpdf <- self$d( x, log = TRUE )
      lcdf <- sapply( seq_along( x ), function( idx ){
        matrixStats::logSumExp( lpdf, idxs = 1 : idx )
      })
      cdf <- exp( lcdf )

      eps <- 10 * .Machine$double.eps
      idx <- findInterval( p - eps, c( 0, cdf[ - length( cdf ) ] ),
                           left.open = FALSE,
                           rightmost.closed = TRUE )

      idx[ p < eps ] <- 1
      idx[ p > 1 - eps ] <- length( cdf )

      return( x[ idx ] )
    }
  ),
  active = list(
    support = function( val ){
      private$.staticReturn( val, "support" )
    }
  )
)

################################################################################/
#  distribution.discrete.binomial
################################################################################/
#' Class: `distribution.discrete.binomial.class`
#' @description Derived class for an binomially-distributed random variable.
#'
#' @param size number of trials (zero or more).
#' @param prob probability of success on each trial.
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
#' @field mean The mean of a binomial distribution with size `$params$size` and
#'   success probability `$params$prob`.
#' @field sd The standard deviation of a binomial distribution with size
#'   `$params$size` and success probability `$params$prob`.
#' @field var The variance of a binomial distribution with size
#'   `$params$size` and success probability `$params$prob`.
distribution.discrete.binomial.class <- R6.class(
  classname = "distribution.discrete.binomial.class",
  inherit   = distribution.discrete.class,
  interfaces = list( distribution.interface ),
  private   = list(
    .name    = "Binomial",
    .param_names = c( "size", "prob" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      # Check that size is an integer >= 0
      #   - Allow numeric size, but verify it is with numeric tolerance
      #     1e-10 of an integer
      if ( params$size < 0 || !is.numeric( params$size ) ||
           ( params$size - round( params$size ) > 1e-10 ) )
        stop( "`params$size` must be an integer >= 0.")
      
      # Check that prob is a numeric value in [0, 1]
      if ( params$prob < 0 || params$prob > 1 || !is.numeric( params$prob ) )
        stop( "`params$prob` must be a numeric value between 0 and 1 (inclusive).")
      
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.discrete.class`
    initialize = function( size, prob ){
      private$.check_params( list( size = size,
                                   prob = prob ) )
      super$initialize( support = c( 0, size ) )
      self$params <- list( size = size,
                           prob = prob )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a binomial random variable with size
    #'   `params$size` and success probability `params$prob`.
    d = function( x, log = FALSE ){
      stats::dbinom( x, size = private$.params$size, prob = private$.params$prob,
                     log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a binomial random variable
    #'   with size `params$size` and success probability `params$prob`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pbinom( q, size = private$.params$size, prob = private$.params$prob,
                     lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a binomial random variable with size
    #'   `params$size` and success probability `params$prob`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qbinom( p, size = private$.params$size, prob = private$.params$prob,
                     lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a binomial random variable
    #'   with size `params$size` and success probability `params$prob`.
    r = function( n ){
      stats::rbinom( n, size = private$.params$size, prob = private$.params$prob )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.params$size * private$.params$prob )
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
      return( private$.params$size * private$.params$prob * ( 1 - private$.params$prob ) )
    }
  )
)

#' distribution.binomial
#' 
#' Constructor function for an object of class [[distribution.discrete.binomial.class]]
#' 
#' @param size number of trials (zero or more).
#' @param prob probability of success on each trial.
#' 
#' @returns An object of class [[distribution.discrete.binomial.class]]
#' 
#' @seealso [Mastiff-Distributions]
#' @export

distribution.binomial <- function( size, prob ){
  distribution.discrete.binomial.class$new( size = size, prob = prob )
}

################################################################################/
#  distribution.discrete.poisson
################################################################################/
#' Class: `distribution.discrete.poisson.class`
#' @description Derived class for an Poisson-distributed random variable.
#'
#' @param lambda vector of (non-negative) means.
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
#' @field mean The mean of a Poisson distribution with mean `$params$lambda`.
#' @field sd The standard deviation of a Poisson distribution with mean
#'   `$params$lambda`.
#' @field var The variance of a Poisson distribution with mean `$params$lambda`.

distribution.discrete.poisson.class <- R6.class(
  classname = "distribution.discrete.poisson.class",
  inherit   = distribution.discrete.class,
  interfaces = list( distribution.interface ),
  private   = list(
    .name    = "poisson",
    .param_names = c( "lambda" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      # Check that params$lambda is a non-negative numeric value
      if ( !is.numeric( params$lambda ) )
        stop( "`params$lambda` must be a numeric value.")
      if ( params$lambda < 0 )
        stop( "`params$lambda` must be >0.")
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.discrete.class`
    initialize = function( lambda ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( lambda = lambda )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a poisson random variable with size
    #'   `params$size` and success probability `params$prob`.
    d = function( x, log = FALSE ){
      stats::dpois( x, lambda = private$.params$lambda,
                    log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a poisson random variable
    #'   with size `params$size` and success probability `params$prob`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::ppois( q, lambda = private$.params$lambda,
                    lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a poisson random variable with size
    #'   `params$size` and success probability `params$prob`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qpois( p, lambda = private$.params$lambda,
                    lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a poisson random variable
    #'   with size `params$size` and success probability `params$prob`.
    r = function( n ){
      stats::rpois( n, lambda = private$.params$lambda )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.params$lambda )
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( sqrt( private$.params$lambda ) )
    },
    ############################################################################/
    # variance
    ############################################################################/
    var = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$var`" )
      return( private$.params$lambda )
    }
  )
)

#' distribution.poisson
#' 
#' Constructor function for an object of class [[distribution.discrete.poisson.class]]
#' 
#' @param lambda vector of (non-negative) means.
#' 
#' @returns An object of class [[distribution.discrete.poisson.class]]
#' 
#' @seealso [Mastiff-Distributions]
#' @export

distribution.poisson <- function( lambda ){
  distribution.discrete.poisson.class$new( lambda = lambda )
}

################################################################################/
#  distribution.discrete.negative_binomial
################################################################################/
#' Class: `distribution.discrete.negative_binomial.class`
#' @description Derived class for an negative binomially-distributed random
#'   variable.
#'
#' @param size target for number of successful trials, or dispersion parameter
#'   (the shape parameter of the gamma mixing distribution). Must be strictly
#'   positive, need not be integer.
#' @param prob probability of success in each trial. 0 < prob <= 1.
#' @param mu alternative parametrization via mean: see [stats::dnbinom]
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
#' @field mean The mean of a negative_binomial distribution with mean `$params$lambda`.
#' @field sd The standard deviation of a negative_binomial distribution with mean
#'   `$params$lambda`.
#' @field var The variance of a negative_binomial distribution with mean `$params$lambda`.

distribution.discrete.negative_binomial.class <- R6.class(
  classname = "distribution.discrete.negative_binomial.class",
  inherit   = distribution.discrete.class,
  interfaces = list( distribution.interface ),
  private   = list(
    .name    = "negative_binomial",
    .param_names = c( "size", "prob", "mu" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      # Check that size is a non-negative integerl; allow numeric input but
      # verify it is close to an integer
      if ( params$size < 0 || !is.numeric( params$size ) )
        stop( "`params$size` must be an integer >= 0.")
      
      
      # Check that prob is a numeric value in [0, 1]
      if ( params$prob < 0 || params$prob > 1 || !is.numeric( params$prob ) )
        stop( "`params$prob` must be a numeric value between 0 and 1 (inclusive).")
      
      # Check that mu is a non-negative numeric value
      if ( params$mu < 0 || !is.numeric( params$mu ) )
        stop( "`params$mu` must be a non-negative numeric value.")
      
      
      # Check that mu and prob are consistent given size
      mean <- params$size * ( 1 - params$prob ) / params$prob
      if ( abs( mean - params$mu ) > 1e-10 )
        stop( "`params$prob` and `params$mu` are inconsistent." )
      
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class
    #'   `distribution.discrete.negative_binomial.class`
    initialize = function( size, prob, mu ){
      super$initialize( support = c( 0, Inf ) )
      self$params <- list( size = size,
                           prob = prob,
                           mu   = mu )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a negative_binomial random variable with size
    #'   `params$size` and success probability `params$prob`.
    d = function( x, log = FALSE ){
      stats::dnbinom( x, size = private$.params$size, prob = private$.params$prob,
                      log = log )
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a negative_binomial random variable
    #'   with size `params$size` and success probability `params$prob`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      stats::pnbinom( q, size = private$.params$size, prob = private$.params$prob,
                      lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a negative_binomial random variable with size
    #'   `params$size` and success probability `params$prob`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      stats::qnbinom( p, size = private$.params$size, prob = private$.params$prob,
                      lower.tail = lower.tail, log.p = log.p )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a negative_binomial random variable
    #'   with size `params$size` and success probability `params$prob`.
    r = function( n ){
      stats::rnbinom( n, size = private$.params$size, prob = private$.params$prob )
    }
  ),
  active = list(
    ############################################################################/
    # params
    ############################################################################/
    # Allow either prob or mu to be input and fill any missing parameters (which
    # can be determined) given input values before dispatching to parent class
    params = function( new_val ){
      if ( missing( new_val ) ) return( private$.params )
      
      if ( is.null( new_val$prob ) && is.null( new_val$mu ) )
        stop( "At least one of `params$prob` and `params$mu` must be set." )
      
      if ( is.null( new_val$prob ) && !is.null( new_val$size ) )
        new_val$prob <- new_val$size / ( new_val$mu + new_val$size )
      
      if ( is.null( new_val$mu ) && !is.null( new_val$size ) )
        new_val$mu <- new_val$size * ( 1 - new_val$prob ) / new_val$prob
      
      super$params <- new_val
    },
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.params$mu )
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
      return( private$.params$mu / private$.params$prob )
    }
  )
)

#' distribution.negative_binomial
#' 
#' Constructor function for an object of class [[distribution.discrete.negative_binomial.class]]
#' 
#' @param size target for number of successful trials, or dispersion parameter
#'   (the shape parameter of the gamma mixing distribution). Must be strictly
#'   positive, need not be integer.
#' @param prob probability of success in each trial. 0 < prob <= 1.
#' @param mu alternative parametrization via mean: see [stats::dnbinom]
#' 
#' @returns An object of class [[distribution.discrete.negative_binomial.class]]
#' 
#' @seealso [Mastiff-Distributions]
#' @export

distribution.negative_binomial <- function( size, prob, mu ){
  if ( missing( prob ) && missing( mu ) )
    stop( "At least one of `prob` and `mu` must be set." )
  
  if ( missing( prob ) && !missing( size ) )
    prob <- size / ( mu + size )
  
  if ( missing( mu ) && !missing( size ) )
    mu <- size * ( 1 - prob ) / prob
  
  distribution.discrete.negative_binomial.class$new( size, prob, mu )
}

################################################################################/
#  distribution.discrete.point_mass
################################################################################/
#' Class: `distribution.discrete.point_mass.class`
#' @description Derived class for a point mass at `$params$value`
#'
#' @param value The point with mass 1.
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
#' @field mean The mean of a point mass at `$params$value`.
#' @field sd The standard deviation of a point mass at `$params$value`.
#' @field var The variance of a point mass at `$params$value`.
distribution.discrete.point_mass.class <- R6.class(
  classname = "distribution.discrete.point_mass.class",
  inherit   = distribution.discrete.class,
  interfaces = list( distribution.interface ),
  private   = list(
    .name    = "point_mass",
    .param_names = c( "value" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      # Check that value is numeric
      if ( !is.numeric( params$value ) ){
        stop( "`$params$value` must by a numeric value.")
      }
      
      return( NULL )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.discrete.class`
    initialize = function( value ){
      super$initialize( support = c( -Inf, Inf ) )
      self$params <- list( value = value )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a point mass at `params$value`.
    d = function( x, log = FALSE ){
      if ( log ){
        ifelse( x == private$.params$value, 0, -Inf )
      } else {
        ifelse( x == private$.params$value, 1, 0 )
      }
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a point mass at
    #'   `params$value`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      if ( log.p ){
        if ( lower.tail ){
          ifelse( q < private$.params$value, -Inf, 0 )
        } else {
          ifelse( q >= private$.params$value, -Inf, 0 )
        }
      } else {
        if ( lower.tail ){
          ifelse( q < private$.params$value, 0, 1 )
        } else {
          ifelse( q >= private$.params$value, 0, 1 )
        }
      }
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a point mass at `params$value`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      if ( log.p ) p <- exp( p )
      if ( !lower.tail ) p <- 1 - p
      
      invalid_p <- is.na( p ) | p < 0 | p > 1
      
      out <- rep( private$.params$value, length( p ) )
      out[ invalid_p ] <- NaN
      return( out )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a point mass at
    #'   `params$value`.
    r = function( n ){
      rep( private$.params$value, n )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.params$value )
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( 0 )
    },
    ############################################################################/
    # variance
    ############################################################################/
    var = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$var`" )
      return( 0 )
    }
  )
)

#' distribution.point_mass
#' 
#' Constructor function for an object of class [[distribution.discrete.point_mass.class]]
#' 
#' @param value The point with mass 1.
#' 
#' @returns An object of class [[distribution.discrete.point_mass.class]]
#' 
#' @seealso [Mastiff-Distributions]
#' @export

distribution.point_mass <- function( value ){
  distribution.discrete.point_mass.class$new( value )
}

################################################################################/
#  distribution.discrete.finite_set
################################################################################/
#' Class: `distribution.discrete.finite_set.class`
#' @description Derived class for a generic discrete distribution on a finite
#'   set of points in (-Inf, Inf).
#'
#' @param prob       vector of probability weights for obtaining each element of
#'   support.
#' @param support    vector of values giving the points in the finite set on
#'   which the distribution has support.
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
#' @field params     Named list of distribution parameters
#' @field support    The support of the distribution, i.e. the subset of values
#'   for which the density is positive,
#' @field mean The mean of a point mass at `$params$value`.
#' @field sd The standard deviation of a point mass at `$params$value`.
#' @field var The variance of a point mass at `$params$value`.
distribution.discrete.finite_set.class <- R6.class(
  classname = "distribution.discrete.finite_set.class",
  inherit   = distribution.discrete.class,
  interfaces = list( distribution.interface ),
  private   = list(
    .name    = "finite_set",
    .param_names = c( "prob" ),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      # Check that the correct number of probabilities are input
      if ( length( params$prob ) != length( private$.support ) )
        stop( "`params$prob` must provide a single probability for each element of `$support`.")
      
      # Check that all probabilities are non-negative numeric values
      if ( !all( is.numeric( params$prob ) ) || !all( params$prob >= 0 ) )
        stop( "All values in `params$prob` must be non-negative probability weights.")
      if ( !is.finite( sum( params$prob ) ) )
        stop( "`params$prob` must have finite sum to allow distribution to be normalised." )
      
      return( NULL )
    },
    .dt_params = NA
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.discrete.class`
    initialize = function( support, prob ){
      if ( any( !is.numeric( support ) ) )
        stop( "All elements of `support` must be numeric values in (-Inf, Inf)." )
      if ( any( duplicated( support ) ) )
        stop( "All elements of `support must be unique." )
      
      super$initialize( support = sort( support ) )
      self$params <- list( prob = prob )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for the probability distribution with mass
    #'   `$params$prob` at points `$support`.
    d = function( x, log = FALSE ){
      x_dt <- data.table::as.data.table( x )
      pdf <- private$.dt_params[ x_dt, on = 'x' ]
      pdf[ is.na( pdf ) & !is.na( x ) ] <- 0
      pdf[ is.na( x ) ] <- NA_real_
      
      if ( log ){
        return( pdf[, ifelse( prob == 0, -Inf, log( prob ) ) ] )
      } else {
        return( pdf[, ifelse( prob == 0, 0, prob ) ] )
      }
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for the probability distribution
    #'   with mass `$params$prob` at points `$support`.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      out <- private$.dt_params[ .(q), on = "x", roll = Inf,
                                 CDF ]
      out[ is.na( out ) & !is.na( q ) ] <- 0
      out[ is.na( q ) ] <- NA_real_
      
      if ( !lower.tail ) out <- 1 - out
      if ( log.p ) out <- log( out )
      
      return( out )
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for the probability distribution with mass
    #'   `$params$prob` at points `$support`.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      if ( log.p ) p <- exp( p )
      if ( !lower.tail ) p <- 1 - p
      
      out <- private$.dt_params[ .(p), on = .(CDF), roll = -Inf, mult = "first",
                                 .( p, x )
      ][ is.na( p ) | p < 0 | p > 1, x := NaN ]
      return( out$x )
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for probability distribution with
    #'   mass `$params$prob` at points `$support`.
    r = function( n ){
      sample( private$.support,
              size = n,
              replace = TRUE,
              prob = private$.params$prob )
    }
  ),
  active = list(
    ############################################################################/
    # support
    ############################################################################/
    support = function( new_val ){
      if ( !missing( new_val ) )
        stop( "`$support` is set at class initialisation and cannot be updated." )
      return( private$.support )
    },
    ############################################################################/
    # params
    ############################################################################/
    params = function( new_val ){
      if ( missing( new_val ) ) return( private$.params )
      super$params <- new_val
      
      # Ensure private$.dt_params is kept up to date with current params
      private$.dt_params <- data.table::data.table(
        x = private$.support,
        prob = self$params$prob
      )[, prob := prob / sum( prob ) ]
      data.table::setkey( private$.dt_params, x )
      private$.dt_params[, CDF := cumsum( prob ) ]
    },
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( private$.dt_params[, sum( x * prob ) ] )
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
      
      mu <- self$mean
      return( private$.dt_params[, sum( (x - mu)^2 * prob ) ] )
    }
  )
)

#' distribution.finite_set
#' 
#' Constructor function for an object of class [[distribution.discrete.finite_set.class]]
#' 
#' @param prob       vector of probability weights for obtaining each element of
#'   support.
#' @param support    vector of values giving the points in the finite set on
#'   which the distribution has support.
#' 
#' @returns An object of class [[distribution.discrete.finite_set.class]]
#' 
#' @seealso [Mastiff-Distributions]
#' @export

distribution.finite_set <- function( prob, support ){
  distribution.discrete.finite_set.class$new( prob = prob, support = support )
}
