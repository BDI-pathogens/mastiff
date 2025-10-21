#' @keywords internal
"_PACKAGE"

# Declare those dataframe column names we use that generate NOTEs
utils::globalVariables(c("label", "value", "density_type", "..params_desired", "density"))

#' The 'mastiff' package.
#'
#' @description TODO: A DESCRIPTION OF THE PACKAGE
#'
#' @name mastiff-package
#' @aliases mastiff
#' @useDynLib mastiff, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom rstantools rstan_config
#' @importFrom RcppParallel RcppParallelLibs
#'
NULL
