
#' logit(p) = log(p / (1 - p)), a wrapper for [stats::qlogis()]
#'
#' @param ... A numeric vector of values between 0 and 1, and other named arguments to [stats::qlogis()] if desired.
#'
#' @returns A numeric vector
#' @export
#'
#' @examples
#' logit(0.5)
#' logit(log(0.5), log.p = TRUE)
logit <- function(...) stats::qlogis(...)

#' logistic(x) = 1 / (1 + exp(-x)), a wrapper for [stats::plogis()]
#'
#' @param ... A numeric vector of values, and other named arguments to [stats::plogis()] if desired.
#'
#' @returns A numeric vector
#' @export
#'
#' @examples
#' logistic(2)
logistic <- function(...) stats::plogis(...)
