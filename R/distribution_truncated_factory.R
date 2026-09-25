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
      lambda <- private$.params$rate

      return(t0 + 1 / lambda - (t1 - t0) / (exp(lambda * (t1 - t0)) - 1))
    },
    ############################################################################/
    # standard deviation
    ############################################################################/
    #' @field mean The standard deviation of an exponential distribution with
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
    #' @field mean The variance of an exponential distribution with rate
    #'   `$params$rate` truncated to `[$t0, $t1]`.
    var = function(val) {
      if (!missing(val)) {
        stop("cannot set `$var`")
      }

      t0 <- private$.t0
      t1 <- private$.t1
      lambda <- private$.params$rate
      
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
#' distribution.truncated.normal.class
distribution.truncated.normal.class <- distribution_truncated_factory(
  classname = "distribution.truncated.normal",
  untruncated_distribution = distribution.normal()
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
  x <- distribution.truncated.normal.class$new(t0 = t0, t1 = t1)
  x$params <- list(mean = mean, sd = sd)
  return(x)
}
