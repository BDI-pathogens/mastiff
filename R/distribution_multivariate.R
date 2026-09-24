################################################################################/
#  distribution.multivariate.class
################################################################################/
#' Class: `distribution.multivariate.class`
#' @description Base class for multivariate distributions
#'
#' @param support The support of the distribution, i.e. the subset of values for
#'   which the density is positive.
#' @param q          matrix of univariate quantiles.
#' @param p          matrix of univariate probabilities.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#' 
#' @field interfaces The list of available class interfaces.
#' @field support    The support of the continuous distribution, i.e. the subset
#'   of values for which the density is positive,
#' @field n_dimensions the number of dimensions of random variable
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
distribution.multivariate.class <- R6.class(
  classname = "distribution.multivariate.class",
  inherit   = distribution.abstract.class,
  private   = list(
    .n_dimensions = 2,
    .support = matrix( c( -Inf, -Inf, Inf, Inf ), ncol = 2 )
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.multivariate.class`
    #' @param n_dimensions the number of dimensions of random variable
    initialize = function( n_dimensions ){
      private$.n_dimensions <- n_dimensions
    }
  ),
  active = list(
    n_dimensions = function( val ){
      private$.staticReturn( val, "n_dimensions" )
    },
    support = function( val ){
      private$.staticReturn( val, "support" )
    }
  )
)

################################################################################/
#  distribution.multivariate.normal.class
################################################################################/
#' Class: `distribution.multivariate.normal.class`
#' @description Base class for multivariate distributions
#'
#' @param support The support of the distribution, i.e. the subset of values for
#'   which the density is positive.
#' @param x          matrix of varlues
#' @param d          vector of univariate densities
#' @param q          matrix of univariate quantiles.
#' @param p          matrix of univariate probabilities.
#' @param n          number of observations. If `length( n ) > 1`, the length is
#'   taken to be the number required.
#' @param log        logical; if TRUE, probabilities p are given as `log(p)`.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#' 
#' @field interfaces The list of available class interfaces.
#' @field mean the means of each variable
#' @field sd the standard deviation of each variable
#' @field var the variance of variable
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
#' 
#' @importFrom mvtnorm dmvnorm
#' @importFrom mvtnorm rmvnorm
distribution.multivariate.normal.class <- R6.class(
  classname = "distribution.multivariate.normal.class",
  inherit   = distribution.multivariate.class,
  private   = list(
    .name    = "Multivarariate-normal",
    .param_names = c( "means", "covariance" ),
    .means = NULL,
    .covariance = NULL,
    ############################################################################/
    # .check_params
    ############################################################################/
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      means        <- params$means
      covariance   <- params$covariance
      n_dimensions <- length( means )
      stopifnot( is.matrix( covariance ) )
      stopifnot( ncol( covariance ) == n_dimensions )
      stopifnot( nrow( covariance ) == n_dimensions )
      
      # check positive semi-definite without importing extra package
      tol <- 1e-8
      stopifnot( isSymmetric( covariance ) )
      evs <- eigen( covariance, only.values = TRUE )$values 
      stopifnot( min( ( Re( evs) ) ) > -tol )
      
      private$.n_dimensions <- n_dimensions
      private$.support <- matrix( c( rep( -Inf, n_dimensions ), 
                          rep( Inf, n_dimensions ) ), ncol = 2 )
      return( params )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.multivariate.normal.class`
    #' @param means vector of means
    #' @param covariance the covariance matrix
    initialize = function( means, covariance ){
      self$params <- list( means = means, covariance = covariance ) 
    },
    ############################################################################/
    # set uniform correlation
    ############################################################################/
    #' @description Updates to the covariance matrix so that all off-diagonal
    #' correlations are the same
    #' @param rho the single correlation between all variables
    set_uniform_correlation = function( rho ){
      stopifnot( rho >= -1 )
      stopifnot( rho <= 1 )
      
      n    <- private$.n_dimensions
      sds  <- sqrt( diag( private$.params$covariance ) )
      corr <- matrix( rho, ncol = n, nrow = n ) + diag( n ) * ( 1 - rho )
      self$params$covariance <- corr * outer( sds, sds )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a multivariate normal
    d = function( x, log = FALSE ){
      stopifnot( length( x ) == private$.n_dimensions )
      mvtnorm::dmvnorm( x, mean = private$.params$means, sigma = private$.params$covariance ) 
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates of a multivariate normal
    #'   with rate `params$rate`.
    r = function( n ){
      mvtnorm::rmvnorm( n, mean = private$.params$means, sigma = private$.params$covariance ) 
    },
    ############################################################################/
    # univariate distribution function
    ############################################################################/
    #' @description Univariate cumulative density functions 
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      if( !is.matrix( q ) ) {
        stopifnot( is.vector( q ) )
        stopifnot( length( q ) == private$.n_dimensions )
        q <- matrix( q, nrow = 1 )
      }
      stopifnot( ncol( q ) == private$.n_dimensions )
      
      means <- private$.params$means 
      sigma <- sqrt( diag( private$.params$covariance ) )
      for( cdx in 1:private$.n_dimensions )
        q[ , cdx ] <- pnorm( q[ , cdx ], mean = means[ cdx ], sd = sigma[ cdx ], lower.tail = lower.tail, log.p = log.p )
      return( q )
    },
    ############################################################################/
    # univariate quantile function
    ############################################################################/
    #' @description Univariate quantile functions 
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      if( !is.matrix( p ) ) {
        stopifnot( is.vector( p ) )
        stopifnot( length( p ) == private$.n_dimensions )
        p <- matrix( p, nrow = 1 )
      }
      stopifnot( ncol( p ) == private$.n_dimensions )
      stopifnot( max( p ) < 1 | min( p ) > 0 )
      
      means <- private$.params$means 
      sigma <- sqrt( diag( private$.params$covariance ) )
      for( cdx in 1:private$.n_dimensions )
        p[ , cdx ] <- qnorm( p[ , cdx ], mean = means[ cdx ], sd = sigma[ cdx ], lower.tail = lower.tail, log.p = log.p )
      return( p )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( self$params$means )
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
      return( diag( self$params$covariance ) )
    }
  )
)

################################################################################/
#' distribution.multivariate_normal
#' 
#' Constructor function for an object of class `distribution.multivariate.normal.class`
#' 
#' @param means the means of the multivariate normal
#' @param covariance the covariance of the multivariate normal
#' 
#' @returns An object of class [[distribution.multivariate.normal.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.multivariate_normal <- function( means, covariance ){
  distribution.multivariate.normal.class$new( means, covariance )
}

################################################################################/
#  distribution.multivariate.t.class
################################################################################/
#' Class: `distribution.multivariate.t.class`
#' @description Base class for multivariate t distribution
#'
#' @param support The support of the distribution, i.e. the subset of values for
#'   which the density is positive.
#' @param x          matrix of varlues
#' @param d          vector of univariate densities
#' @param q          matrix of univariate quantiles.
#' @param p          matrix of univariate probabilities.
#' @param n          number of observations. If `length( n ) > 1`, the length is
#'   taken to be the number required.
#' @param log        logical; if TRUE, probabilities p are given as `log(p)`.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}.
#' 
#' @field interfaces The list of available class interfaces.
#' @field mean the means of each variable
#' @field sd the standard deviation of each variable
#' @field var the variance of variable
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
#' 
#' @importFrom mvtnorm dmvt
#' @importFrom mvtnorm rmvt
distribution.multivariate.t.class <- R6.class(
  classname = "distribution.multivariate.t.class",
  inherit   = distribution.multivariate.class,
  private   = list(
    .name    = "Multivarariate-tl",
    .param_names = c( "means", "scale_matrix", "df" ),
    .means = NULL,
    .covariance = NULL,
    ############################################################################/
    # .check_params
    ############################################################################/
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      means        <- params$means
      scale_matrix   <- params$scale_matrix
      df           <- params$df
      n_dimensions <- length( means )
      stopifnot( is.matrix( scale_matrix ) )
      stopifnot( ncol( scale_matrix ) == n_dimensions )
      stopifnot( nrow( scale_matrix ) == n_dimensions )
      stopifnot( df >= 0 )
      
      # check positive semi-definite without importing extra package
      tol <- 1e-8
      stopifnot( isSymmetric( scale_matrix ) )
      evs <- eigen( scale_matrix, only.values = TRUE )$values 
      stopifnot( min( ( Re( evs) ) ) > -tol )
      
      private$.n_dimensions <- n_dimensions
      private$.support <- matrix( c( rep( -Inf, n_dimensions ), 
                                     rep( Inf, n_dimensions ) ), ncol = 2 )
      return( params )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.multivariate.normal.class`
    #' @param means vector of means 
    #' @param scale_matrix the scale_matrix matrix
    #' @param df the degrees of freedom
    initialize = function( means, scale_matrix, df ){
      self$params <- list( means = means, scale_matrix = scale_matrix, df = df ) 
    },
    ############################################################################/
    # set uniform correlation
    ############################################################################/
    #' @description Updates to the scale_matrix matrix so that all off-diagonal
    #' correlations are the same
    #' @param rho the single correlation between all variables
    set_uniform_correlation = function( rho ){
      stopifnot( rho >= -1 )
      stopifnot( rho <= 1 )
      
      n    <- private$.n_dimensions
      sds  <- sqrt( diag( private$.params$scale_matrix ) )
      corr <- matrix( rho, ncol = n, nrow = n ) + diag( n ) * ( 1 - rho )
      self$params$scale_matrix <- corr * outer( sds, sds )
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a multivariate normal
    d = function( x, log = FALSE ){
      stopifnot( length( x ) == private$.n_dimensions )
      mvtnorm::dmvt( x, delta = private$.params$means, sigma = private$.params$scale_matrix, df = private$.params$df ) 
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates of a multivariate normal
    #'   with rate `params$rate`.
    r = function( n ){
      mvtnorm::rmvt( n, delta = private$.params$means, sigma = private$.params$scale_matrix, df = private$.params$df ) 
    },
    ############################################################################/
    # univariate distribution function
    ############################################################################/
    #' @description Univariate cumulative density functions 
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      if( !is.matrix( q ) ) {
        stopifnot( is.vector( q ) )
        stopifnot( length( q ) == private$.n_dimensions )
        q <- matrix( q, nrow = 1 )
      }
      stopifnot( ncol( q ) == private$.n_dimensions )
      
      means <- private$.params$means 
      sigma <- sqrt( diag( private$.params$scale_matrix ) )
      for( cdx in 1:private$.n_dimensions )
        q[ , cdx ] <- pt( ( q[ , cdx ] - means[ cdx ] ) / sigma[ cdx ], self$params$df, 0,
                          lower.tail = lower.tail, log.p = log.p )
      return( q )
    },
    ############################################################################/
    # univariate quantile function
    ############################################################################/
    #' @description Univariate quantile functions 
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      if( !is.matrix( p ) ) {
        stopifnot( is.vector( p ) )
        stopifnot( length( p ) == private$.n_dimensions )
        p <- matrix( p, nrow = 1 )
      }
      stopifnot( ncol( p ) == private$.n_dimensions )
      stopifnot( max( p ) < 1 | min( p ) > 0 )
      
      means <- private$.params$means 
      sigma <- sqrt( diag( private$.params$scale_matrix ) )
      for( cdx in 1:private$.n_dimensions )
        p[ , cdx ] <- ( qt( p[ , cdx ], self$params$df, lower.tail = lower.tail, log.p = log.p ) *
                        sigma[ cdx ] ) + means[ cdx ]
      return( p )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( self$params$means )
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
      df <- self$params$df
      if( df <= 2 )
        return( rep( Inf, ncol( self$params$scale_matrix ) ) )
      return( diag( self$params$scale_matrix ) * df / (df - 2 )  )
    }
  )
)

################################################################################/
#' distribution.multivariate_t
#' 
#' Constructor function for an object of class `distribution.multivariate.t.class`
#' 
#' @param means the means of the multivariate t-distribution
#' @param scale_matrix the scale matrix of the multivariate t-distribution (cov=scale*dt/(df-2))
#' @param df the degrees of freedom for the t
#' 
#' @returns An object of class [[distribution.multivariate.t.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.multivariate_t <- function( means, scale_matrix, df ){
  distribution.multivariate.t.class$new( means, scale_matrix, df )
}


################################################################################/
#  distribution.multivariate.copulal.class
################################################################################/
#' Class: `distribution.multivariate.copual.class`
#' @description Class for copula distributions
#'
#' @param support The support of the distribution, i.e. the subset of values for
#'   which the density is positive.
#' @param x          matrix of varlues
#' @param d          vector of univariate densities
#' @param q          matrix of univariate quantiles.
#' @param p          matrix of univariate probabilities.
#' @param n          number of observations. If `length( n ) > 1`, the length is
#'   taken to be the number required.
#' @param log        logical; if TRUE, probabilities p are given as `log(p)`.
#' @param log.p      logical; if TRUE, probabilities p are given as `log(p)`.
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[ X \leq x ]},
#'   otherwise, \eqn{P[X>x]}
#' 
#' @field interfaces The list of available class interfaces.
#' @field mean the means of each variable
#' @field sd the standard deviation of each variable
#' @field var the variance of variable
#' @field distributions the univariate distributions in the copula
#' @field copula the multivariate copula distribution generating the correlation
#' between random variables
#' 
#' @include R6_class.R
#' @include distribution_R6_class.R
#' 
distribution.multivariate.copula.class <- R6.class(
  classname = "distribution.multivariate.copula.class",
  inherit   = distribution.multivariate.class,
  private   = list(
    .distributions = NULL,
    .copula = NULL
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.multivaraite.copula.class`
    #' @param distributions the univariate distributions in the copula
    #' @param copula the multivariate copula distribution generating the correlation
    #' between random variables
    initialize = function( distributions, copula ){
      stopifnot( is.list( distributions ) )
      lapply( distributions, function( d ) stopifnot( inherits( d, "distribution.abstract.class") ) )
      stopifnot( inherits( copula, "distribution.multivariate.class") )
      stopifnot( length( distributions ) == copula$n_dimensions )
      
      super$initialize( copula$n_dimensions )
      private$.distributions <- distributions 
      private$.copula <- copula
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates of a multivariate normal
    #'   with rate `params$rate`.
    r = function( n ){
      r_copula <- self$copula$r( n )
      p_copula <- self$copula$p( r_copula )
      dists    <- self$distributions
      for( cdx in 1:self$n_dimensions )
        p_copula[ , cdx ] <- dists[[ cdx ]]$q( p_copula[ , cdx ] )
      p_copula
    }
  ),
  active = list(
    ############################################################################/
    # distributions 
    ############################################################################/
    distributions = function( val ){
      private$.staticReturn( val, "distributions" )
    },
    ############################################################################/
    # distributions 
    ############################################################################/
    copula = function( val ){
      private$.staticReturn( val, "copula" )
    },
    ############################################################################/
    # mean
    ############################################################################/
    mean = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$mean`" )
      return( unlist( lapply( self$distributions, function( x ) x$mean ) ) )
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    sd = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$sd`" )
      return( unlist( lapply( self$distributions, function( x ) x$sd ) ) )
    },
    ############################################################################/
    # variance
    ############################################################################/
    var = function( val ){
      if( !missing( val ) )
        stop( "cannot set `$var`" )
      return( unlist( lapply( self$distributions, function( x ) x$var ) ) )
    }
  )
)

################################################################################/
#' distribution.copula
#' 
#' Constructor function for an object of class `distribution.multivariate.copula.class`
#' 
#' @param distributions a list of univarite distributions
#' @param copula a multivariate distribution to generate the jointly distributed variales
#' 
#' @returns An object of class [[distribution.multivariate.copula.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.copula <- function( distributions, copula ){
  distribution.multivariate.copula.class$new( distributions, copula )
}

################################################################################/
#' distribution.copula_gaussian
#' 
#' Constructor function for a Gaussian copula
#' 
#' @param distributions a list of univarite distributions
#' @param rho single correlation in the copula
#' 
#' @returns An object of class [[distribution.multivariate.copula.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.copula_gaussian <- function( distributions, rho ){
  n_dim   <- length( distributions )
  cov_mat <- matrix( rho, nrow = n_dim, ncol = n_dim ) + diag( 1 - rho, nrow = n_dim, ncol = n_dim )
  copula  <- distribution.multivariate_normal( rep( 0, n_dim ), cov_mat )
  distribution.copula( distributions, copula )
}

################################################################################/
#' distribution.copula_t
#' 
#' Constructor function for a t-copula
#' 
#' @param distributions a list of univarite distributions
#' @param rho single correlation in scale matrix of the t
#' @param df degrees of freedom in the t
#' 
#' @returns An object of class [[distribution.multivariate.copula.class]]
#'
#' @seealso [Mastiff-Distributions]
#' @export
distribution.copula_t <- function( distributions, rho, df ){
  n_dim   <- length( distributions )
  sca_mat <- matrix( rho, nrow = n_dim, ncol = n_dim ) + diag( 1 - rho, nrow = n_dim, ncol = n_dim )
  copula  <- distribution.multivariate_t( rep( 0, n_dim ), sca_mat, df  )
  distribution.copula( distributions, copula )
}


