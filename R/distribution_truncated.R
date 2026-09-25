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
    .distribution = NULL,
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
    #' @param distribution the untruncated distribution
    #' @param t0 Lower bound for truncation
    #' @param t1 Upper bound for truncation
    initialize = function(distribution,
                          t0 = distribution$support[1],
                          t1 = distribution$support[2]) {
      if (missing(distribution)){
        if (!is.null(private$.distribution)){
          distribution <- private$.distribution
        } else {
          stop('argument "distribution" is missing, with no default')
        }
      }
      
      if (!is.distribution(distribution))
        stop("`distribution` must be a mastiff R6 distribution class")
      
      # Set up the untruncated distribution
      private$.distribution <- distribution
      private$.params       <- distribution$params
      private$.param_names  <- distribution$param_names
      private$.support      <- distribution$support
      
      # Initialise as an untruncated distribution
      private$.t0 <- distribution$support[1]
      private$.t1 <- distribution$support[2]
      private$.normalisation_lower    <- private$.distribution$p(distribution$support[1])
      private$.normalisation_upper    <- private$.distribution$p(distribution$support[2])
      private$.normalisation_constant <- private$.normalisation_upper - private$.normalisation_lower
      
      # Update truncation - active bindings handle input checks
      self$t0 <- t0
      self$t1 <- t1
    },
    ############################################################################/
    # density
    ############################################################################/
    #' @description Density function for a truncated random variable.
    d = function( x, log = FALSE ){
      d <- private$.distribution$d(x, log = FALSE) / private$.normalisation_constant
      d[x <= private$.t0 | x >= private$.t1] <- 0 # Set density to 0 outside of truncated support
      return(d)
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
      if (log.p) p <- exp(p)
      if (!lower.tail) p <- 1 - p
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
        stop("`$t0` must be a numeric value less than `$t1`.")
      
      if (new_val < private$.distribution$support[1]){
        warning(sprintf(
          "Lower truncation at %g has no effect when untruncated distribution has lower support %f",
          new_val, private$.distribution$support[1]
        ))
      }
      
      # New t0 value is restricted to lie within the support of the untruncated distribution
      new_t0                     <- max(new_val, private$.distribution$support[1])
      new_normalisation_lower    <- private$.distribution$p(new_t0)
      new_normalisation_constant <- private$.normalisation_upper - new_normalisation_lower
      
      if (new_normalisation_constant == 0)
        stop(sprintf(
          "Untruncated distribution has no mass on [%g, %g].",
          new_t0, private$.t1))
      if (new_normalisation_constant < 1e-10)
        warning(
          sprintf(
            "Untruncated distribution has almost no mass on [%g, %g] - proceed with caution.",
            new_t0, private$.t1
          )
        )
      
      private$.t0 <- new_t0
      private$.support[1] <- new_t0
      # Update the normalisation constant 
      private$.normalisation_lower    <- new_normalisation_lower
      private$.normalisation_constant <- new_normalisation_constant
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
        stop("`$t1` must be a numeric value greater than `$t0`.")
      
      if (new_val > private$.distribution$support[2])
        warning(
          sprintf(
            "Upper truncation at %g has no effect when untruncated distribution has upper support %g",
            new_val, private$.distribution$support[2]
          )
        )
      
      # New t1 value is restricted to lie within the support of the untruncated distribution
      new_t1                     <- min(new_val, private$.distribution$support[2])
      new_normalisation_upper    <- private$.distribution$p(new_t1)
      new_normalisation_constant <- new_normalisation_upper - private$.normalisation_lower
      
      if (new_normalisation_constant == 0)
        stop(sprintf(
          "Untruncated distribution has no mass on [%g, %g].",
          private$.t0, new_t1))
      if (new_normalisation_constant < 1e-10)
        warning(
          sprintf(
            "Untruncated distribution has almost no mass on [%g, %g] - proceed with caution.",
            private$.t0, new_t1
          )
        )
      
      private$.t1 <- new_t1
      private$.support[2] <- new_t1
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

#' distribution.truncated
#' 
#' Constructor function for an object of class [[distribution.truncated.class]]
#' 
#' @param distribution the untruncated distribution
#' @param t0,t1 Upper and lower bound for truncation
#' 
#' @returns An object of class [[distribution.truncated.class]]
#'
#' @export

distribution.truncated <- function(distribution,
                                   t0 = distribution$support[1],
                                   t1 = distribution$support[2]){
  return(
    distribution.truncated.class$new(
      distribution, t0, t1
    )
  )
}

################################################################################/
# Truncated exponential distribution
################################################################################/
#' Class: `distribution.truncated.exponential.class`
#' @description Derived class for an exponentially-distributed random variable
#'   with interval truncation.
distribution.truncated.exponential.class <- R6.class(
  classname = "distribution.truncated.exponential.class",
  inherit = distribution.truncated.class,
  private = list(
    .name = "TruncatedExponential",
    .param_names = c("rate"),
    .check_params = function(params) {
      # Check that params contains all elements of private$.param_names
      super$.check_params(params)
      
      # Check that params$rate is a non-negative numeric value
      if (!is.numeric(params$rate)) {
        stop("`params$rate` must be a numeric value.")
      }
      if (params$rate < 0) {
        stop("`params$rate` must be >0.")
      }
      
      return(params)
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.truncated.exponential.class`
    #' 
    #' @param rate The rate of the exponential distribution
    #' @param t0 Lower bound for truncation
    #' @param t1 Upper bound for truncation
    initialize = function(rate = 1, t0 = 0, t1 = Inf) {
      super$initialize(
        distribution.exponential(rate = rate, offset = 0),
        t0 = t0,
        t1 = t1
      )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    #' @field mean The mean of an exponential distribution with rate
    #'   `$params$rate` truncated to `[$t0, $t1]`.
    mean = function(val) {
      if (!missing(val)) {
        stop("cannot set `$mean`")
      }
      
      t0 <- private$.t0
      t1 <- private$.t1
      lambda <- self$params$rate
      
      return(t0 + 1 / lambda - (t1 - t0) / (exp(lambda * (t1 - t0)) - 1))
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    #' @field sd The standard deviation of an exponential distribution with
    #'   rate `$params$rate` truncated to `[$t0, $t1]`.
    sd = function(val) {
      if (!missing(val)) {
        stop("cannot set `$sd`")
      }
      return(sqrt(self$var))
    },
    ############################################################################/
    # variance
    ############################################################################/
    #' @field var The variance of an exponential distribution with rate
    #'   `$params$rate` truncated to `[$t0, $t1]`.
    var = function(val) {
      if (!missing(val)) {
        stop("cannot set `$var`")
      }
      
      t0 <- private$.t0
      t1 <- private$.t1
      lambda <- self$params$rate
      
      return(1 / lambda^2 - (t1 - t0)^2 * exp(lambda * (t1 - t0)) /
               (exp(lambda * (t1 - t0)) - 1)^2)
    }
  )
)

#' distribution.truncated.exponential
#'
#' Constructor function for a random variable distributed under a truncated
#' exponential distribution.
#'
#' @inheritParams distribution.exponential rate
#' @param t0,t1 Lower and upper limits for truncation
#'
#' @returns An object of class [distribution.truncated.exponential.class]
#' @export
distribution.truncated.exponential <- function(rate = 1, t0 = 0, t1 = Inf) {
  distribution.truncated.exponential.class$new(rate = rate, t0 = t0, t1 = t1)
}

################################################################################/
# Truncated normal distribution
################################################################################/
#' Class: `distribution.truncated.normal.class`
#' @description Derived class for an normally-distributed random variable
#'   with interval truncation.
distribution.truncated.normal.class <- R6.class(
  classname = "distribution.truncated.normal.class",
  inherit = distribution.truncated.class,
  private = list(
    .name = "TruncatedNormal",
    .param_names = c("mean", "sd"),
    .check_params = function( params ){
      # Check that params contains all elements of private$.param_names
      super$.check_params( params )
      
      if ( !is.numeric( params$mean ) )
        stop( "`params$mean` must be a numeric value.")
      if ( params$sd < 0 || !is.numeric( params$sd ) )
        stop( "`params$sd` must be a non-negative numeric value.")
      
      return( params )
    }
  ),
  public = list(
    ############################################################################/
    # initialize
    ############################################################################/
    #' @description Create a new object of class `distribution.truncated.normal.class`
    #' 
    #' @param mean The mean of the untruncated normal distribution.
    #' @param sd The standard deviation of the untruncated normal distribution.
    #' @param t0 Lower bound for truncation
    #' @param t1 Upper bound for truncation
    initialize = function(mean = 0, sd = 1, t0 = 0, t1 = Inf) {
      super$initialize(
        distribution.normal(mean = mean, sd = sd),
        t0 = t0,
        t1 = t1
      )
    }
  ),
  active = list(
    ############################################################################/
    # mean
    ############################################################################/
    #' @field mean The mean of a normal distribution with mean `$params$mean`
    #'   and standard deviation `$params$sd` truncated to `[$t0, $t1]`.
    mean = function(val) {
      if (!missing(val)) {
        stop("cannot set `$mean`")
      }
      
      t0 <- private$.t0
      t1 <- private$.t1
      mu <- self$params$mean
      sigma <- self$params$sd
      
      alpha <- (t0 - mu) / sigma
      beta  <- (t1 - mu) / sigma
      Z     <- pnorm(beta) - pnorm(alpha)
      
      return(mu + sigma * (dnorm(alpha) - dnorm(beta)) / Z)
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    #' @field sd The standard deviation of a normal distribution with mean
    #'   `$params$mean` and standard deviation `$params$sd` truncated to 
    #'   `[$t0, $t1]`.
    sd = function(val) {
      if (!missing(val)) {
        stop("cannot set `$sd`")
      }
      return(sqrt(self$var))
    },
    ############################################################################/
    # variance
    ############################################################################/
    #' @field var The variance of a normal distribution with mean `$params$mean`
    #'   and standard deviation `$params$sd` truncated to `[$t0, $t1]`.
    var = function(val) {
      if (!missing(val)) {
        stop("cannot set `$var`")
      }
      
      t0 <- private$.t0
      t1 <- private$.t1
      mu <- self$params$mean
      sigma <- self$params$sd
      
      alpha <- (t0 - mu) / sigma; dalpha <- dnorm(alpha)
      beta  <- (t1 - mu) / sigma; dbeta  <- dnorm(beta)
      Z     <- pnorm(beta) - pnorm(alpha)
      
      return(sigma^2 * (1 - (beta * dbeta - alpha * dalpha) / Z -
                          ((dalpha - dbeta) / Z)^2))
    }
  )
)

#' distribution.truncated.normal
#'
#' Constructor function for a random variable distributed under a truncated
#' normal distribution.
#'
#' @inheritParams distribution.normal mean sd
#' @param t0,t1 Lower and upper limits for truncation
#'
#' @returns An object of class [distribution.truncated.normal.class]
#' @export
distribution.truncated.normal <- function(mean = 0, sd = 1, t0 = 0, t1 = Inf) {
  distribution.truncated.normal.class$new(
    mean = mean, sd = sd, t0 = t0, t1 = t1
  )
}