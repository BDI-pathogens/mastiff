# distribution_truncated_factory
#
# Factory function to generate truncated distribution classes
#
# @param classname The classname for the derived truncated distribution class
# @param untruncated_distribution Mastiff R6 distribution class giving the
#   untruncated version of the distribution
# @param func_mean Function to evaluate the mean of the distribution using the
#   R6 structure of the truncated distribution. The default evaluates the mean
#   using numerical integration, but can be overwritten if an analytic form is
#   known
# @param func_sd Function to evaluate the standard deviation of the distribution
#   using the R6 structure of the truncated distribution. The default evaluates
#   the standard deviation as the square root of the variance, but can be
#   overwritten if an analytic form is known
# @param func_var Function to evaluate the variance of the distribution using
#   the R6 structure of the truncated distribution. The default evaluates the
#   variance using numerical integration, but can be overwritten if an analytic
#   form is known
#
# @returns A R6 class generator which can be initialised into a full R6
#   truncated distribution class

distribution_truncated_factory <- function(
    classname,
    untruncated_distribution,
    func_mean = function(self) integrate(function(x) x * self$d(x),
                                         lower = self$support[1],
                                         upper = self$support[2])$value,
    func_sd = function(self) sqrt(self$var),
    func_var = function(self){
      mu <- self$mean
      integrate(function(x) (x - mu)^2 * self$d(x),
                lower = self$support[1],
                upper = self$support[2])$value
    }
){
  R6.class(
    classname = classname,
    inherit = distribution.truncated.class,
    private = list(
      .distribution = untruncated_distribution
    ),
    active = list(
      mean = function(val){
        if (!missing(val))
          stop("`$mean()` cannot be set.")
        func_mean(self)
      },
      sd = function(val){
        if (!missing(val))
          stop("`$sd()` cannot be set.")
        func_sd(self)
      },
      var = function(val){
        if (!missing(val))
          stop("`$var()` cannot be set.")
        func_var(self)
      }
    )
  )
}

################################################################################/
# Truncated exponential distribution
################################################################################/
#' distribution.truncated.exponential.class
distribution.truncated.exponential.class <- distribution_truncated_factory(
  classname = "distribution.truncated.exponential",
  untruncated_distribution = distribution.exponential()
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
distribution.truncated.exponential <- function(rate = 1, t0 = 0, t1 = Inf){
  x <- distribution.truncated.exponential.class$new(t0 = t0, t1 = t1)
  x$params$rate <- rate
  return(x)
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
distribution.truncated.normal <- function(mean = 0, sd = 1, t0 = 0, t1 = Inf){
  x <- distribution.truncated.normal.class$new(t0 = t0, t1 = t1)
  x$params <- list(mean = mean, sd = sd)
  return(x)
}
