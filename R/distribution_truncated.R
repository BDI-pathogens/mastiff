################################################################################ /
#  distribution.truncated.class
################################################################################ /
#' Class: `distribution.truncated.class`
#' @description Base class for truncated distributions
#' @include R6_class.R
#' @include distribution_R6_class.R
distribution.truncated.class <- R6.class(
  classname = "distribution.truncated.class",
  inherit   = distribution.abstract.class,
  private   = list(
    .distribution = distribution.exponential(), # Placeholder distribution
    .t0 = -Inf,
    .t1 = Inf,
    .support = c(-Inf, Inf),
    .normalisation_lower = NA_real_,
    .normalisation_upper = NA_real_,
    .normalisation_constant = NA_real_
  ),
  public = list(
    ############################################################################ /
    # initialize
    ############################################################################ /
    #' @description Create a new object of class `distribution.multivariate.class`
    #' @param distribution
    #' @param t0 Lower bound for truncation [default -Inf]
    #' @param t1 Upper bound for truncation [default Inf]
    initialize = function(distribution, t0 = -Inf, t1 = Inf) {
      # Set up the untruncated distribution
      private$.distribution <- distribution
      private$.params       <- distribution$params
      private$.param_names  <- distribution$param_names
      
      private$.t0 <- t0
      private$.t1 <- t1
      
      # Update normalisation constant
      private$.normalisation_lower    <- private$.distribution$p(t0)
      private$.normalisation_upper    <- private$.distribution$p(t1)
      private$.normalisation_constant <- private$.normalisation_upper - private$.normalisation_lower
      
      if (private$.normalisation_constant == 0)
        stop("Untruncated distribution has no mass between `t0` and new `t1` value.")
      if (private$.normalisation_constant < 1e-10)
        warning("Untruncated distribution has almost no mass between `t0` and `t1` - proceed with caution.")
      
      private$.support <- c(private$.t0, private$.t1)
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a truncated random variable.
    d = function( x, log = FALSE ){
      private$.distribution$d(x, log = FALSE) / private$.normalisation_constant
    },
    ############################################################################/
    # distribution function
    ############################################################################/
    #' @description Cumulative density function for a truncated random variable.
    p = function( q, lower.tail = TRUE, log.p = FALSE ){
      p <- (private$.distribution$p(q, lower.tail = TRUE, log.p = FALSE) - private$.normalisation_lower) /
        private$.normalisation_constant
      
      p[q <= private$.t0] <- 0 # P[X <= t0] = 0
      p[q >= private$.t1] <- 1 # P[X <= t1] = 1
      
      if (!lower.tail) p <- 1 - p
      if (log.p) p <- log(p)
      return(p)
    },
    ############################################################################/
    # quantile function
    ############################################################################/
    #' @description Quantile function for a truncated random variable.
    q = function( p, lower.tail = TRUE, log.p = FALSE ){
      if (!lower.tail) p <- 1 - p
      if (log.p) p <- exp(p)
      private$.distribution$q(p * private$.normalisation_constant +
                                private$.normalisation_lower,
                              lower.tail = TRUE, log.p = FALSE)
    },
    ############################################################################/
    # random deviates
    ############################################################################/
    #' @description Generates random deviates for a truncated random variable by
    #'   inversion sampling from the truncated CDF.
    r = function( n ){
      U <- stats::runif(n,
                        min = private$.normalisation_lower,
                        max = private$.normalisation_upper)
      private$.distribution$q(U, lower.tail = TRUE, log.p = FALSE)
    }
  ),
  active = list(
    ############################################################################/
    # distribution
    ############################################################################/
    #' @field distribution the untruncated distribution
    distribution = function(val){
      private$.staticReturn(val, "distribution")
    },
    ############################################################################/
    # params
    ############################################################################/
    params = function(new_val){
      if (missing(new_val))
        return(private$.params)
      
      # Pass responsibility for parameter checks to the untruncated distribution
      private$.distribution$params <- new_val
      private$.params <- private$.distribution$params
    },
    ############################################################################/
    # t0
    ############################################################################/
    #' @field t0 the lower bound for truncation. The distribution is truncated
    #'   to have support `[t0, t1]`
    t0 = function(new_val){
      if (missing(new_val))
        return(private$.t0)
      
      if (!is.numeric(new_val) | new_val > self$t1)
        stop("`t0` must be a numeric value less than `t1`.")
      
      new_normalisation_lower    <- private$.distribution$p(new_val)
      new_normalisation_constant <- private$.normalisation_upper - new_normalisation_lower
      
      if (new_normalisation_constant == 0)
        stop("Untruncated distribution has no mass between `t0` and new `t1` value.")
      if (new_normalisation_constant < 1e-10)
        warning("Untruncated distribution has almost no mass between `t0` and `t1` - proceed with caution.")
      
      private$.t0 <- new_val
      private$.support <- c(private$.t0, private$.t1)
      # Update the normalisation constant 
      private$.normalisation_lower <- private$.distribution$p(private$.t0)
      private$.normalisation_constant <- private$.normalisation_upper - private$.normalisation_lower
    },
    ############################################################################/
    # t1
    ############################################################################/
    #' @field t1 the upper bound for truncation. The distribution is truncated
    #'   to have support `[t0, t1]`
    t1 = function(new_val){
      if (missing(new_val))
        return(private$.t1)
      
      if (!is.numeric(new_val) | new_val < self$t0)
        stop("`t1` must be a numeric value greater than `t0`.")
      
      new_normalisation_upper    <- private$.distribution$p(new_val)
      new_normalisation_constant <- new_normalisation_upper - private$.normalisation_lower
      
      if (new_normalisation_constant == 0)
        stop("Untruncated distribution has no mass between `t0` and new `t1` value.")
      if (new_normalisation_constant < 1e-10)
        warning("Untruncated distribution has almost no mass between `t0` and `t1` - proceed with caution.")
      
      private$.t1 <- new_val
      private$.support <- c(private$.t0, private$.t1)
      # Update the normalisation constant 
      private$.normalisation_upper    <- new_normalisation_upper
      private$.normalisation_constant <- new_normalisation_constant
    },
    ############################################################################/
    # support
    ############################################################################/
    #' @field support    The support of the truncated distribution, i.e. the
    #'   subset of values for which the density is positives
    support = function( val ){
      private$.staticReturn( val, "support" )
    }
  )
)
