#' Converts a stanfit object to a matrix or data.table
#'
#' @param stanfit A stanfit object e.g. as returned by [rstan::sampling()].
#' @param params_desired A character vector with the names of those parameters
#'   in the stanfit object that should be included in the output. The default
#'   value of `NA` means all parameters are included except `"lp__"` (which is not
#'   actually a parameter: Stan uses it to record the value of the target
#'   probability density).
#' @returns A matrix or data.table with one column per parameter and one row per
#'   sample.
#' @examples
#'   eg <- mastiff::stan_example_regression$posterior_samples
#'   stanfit_to_matrix(eg)
#'   stanfit_to_dt(eg)
#' @export
stanfit_to_matrix <- function( stanfit,
                               params_desired = NA ) {
  stopifnot( class( stanfit )[[ 1 ]] == "stanfit" )
  stanfit_as_matrix <- as.matrix( stanfit )
  if ( ! identical(NA, params_desired ) ) {
    stopifnot( is.character( params_desired ) )
    stopifnot( length( params_desired ) > 0L )
    if (anyDuplicated(params_desired)) {
      warning("Duplicates are present in params_desired. Ignoring them.")
      params_desired <- unique(params_desired)
    }
    for (param in params_desired) {
      if (! param %in% colnames( stanfit_as_matrix ) ) {
        stop(paste("Parameter", param, "not present in stanfit object"))
      }
    }
    stanfit_as_matrix <- stanfit_as_matrix[ , params_desired, drop = FALSE ]
  } else {
    stanfit_as_matrix <-
      stanfit_as_matrix[ , colnames( stanfit_as_matrix ) != "lp__", drop = FALSE ]
  }
  stanfit_as_matrix
}

#' @rdname stanfit_to_matrix
#' @export
stanfit_to_dt <- function( stanfit,
                           params_desired = NA ) {
  stopifnot( class( stanfit )[[ 1 ]] == "stanfit" )
  stanfit_as_dt <- data.table::as.data.table( stanfit )
  if ( ! identical(NA, params_desired ) ) {
    stopifnot( is.character( params_desired ) )
    stopifnot( length( params_desired ) > 0L )
    if (anyDuplicated(params_desired)) {
      warning("Duplicates are present in params_desired. Ignoring them.")
      params_desired <- unique(params_desired)
    }
    for (param in params_desired) {
      if (! param %in% colnames( stanfit_as_dt ) ) {
        stop(paste("Parameter", param, "not present in stanfit object"))
      }
    }
    stanfit_as_dt <- stanfit_as_dt[ , params_desired, with = FALSE ]
  } else {
    stanfit_as_dt <-
      stanfit_as_dt[ , colnames( stanfit_as_dt ) != "lp__", with = FALSE ]
  }
  stanfit_as_dt
}

#' Calculates the mean or median for each parameter for each stanfit object
#'
#' In presto the typical list supplied to this would have one stanfit object
#' for each simulated trial.
#'
#' @param stanfit_list A list of stanfit objects e.g. as returned by
#'   [rstan::sampling()]. The function's name reflects the assumption that these
#'   contain samples from the posterior, but they could contain samples from the
#'   prior.
#' @param params_desired A character vector with the names of those parameters
#'   in the stanfit object that should be included in the output. The default
#'   value of `NA` means all parameters are included except `"lp__"` (which is not
#'   actually a parameter: Stan uses it to record the value of the target
#'   probability density).
#'
#' @returns A 2D array: the first index for the parameter, the second index for
#'    the stanfit object.
#' @examples
#'    eg <- mastiff::stan_example_regression
#'    posterior_means(  list(eg$posterior_samples, eg$prior_samples))
#'    posterior_medians(list(eg$posterior_samples, eg$prior_samples))
#' @export
posterior_means <- function( stanfit_list,
                             params_desired = NA ) {
  stopifnot( is.list( stanfit_list ) )
  results_list <- lapply( stanfit_list, function( samples_one_stanfit ) {
    samples_one_stanfit <-
      stanfit_to_matrix( samples_one_stanfit, params_desired )
    apply( samples_one_stanfit, 2, mean )
  } )
  simplify2array( results_list )
}

#' @rdname posterior_means
#' @export
posterior_medians <- function( stanfit_list,
                               params_desired = NA ) {
  stopifnot( is.list( stanfit_list ) )
  results_list <- lapply( stanfit_list, function( samples_one_stanfit ) {
    samples_one_stanfit <-
      stanfit_to_matrix( samples_one_stanfit, params_desired )
    apply( samples_one_stanfit, 2, stats::median )
  } )
  simplify2array( results_list )
}

#' Calculates central probability intervals for each parameter for each stanfit
#' object
#'
#' The function is a trivial wrapper around [rstantools::posterior_interval()]
#' for a list of stanfit objects.
#'
#' @param stanfit_list A list of stanfit objects e.g. as returned by
#'   [rstan::sampling()]. The function's name reflects the assumption that these
#'   contain samples from the posterior, but they could contain samples from the
#'   prior.
#' @param prob A single number between 0 and 1 specifying the amount of
#'   probability to be contained in the interval. e.g. Specifying 0.95 results
#'   in an interval that spans the 2.5th to 97.5th percentiles of the
#'   distribution.
#' @param params_desired A character vector with the names of those parameters
#'   in the stanfit object that should be included in the output. The default
#'   value of `NA` means all parameters are included except `"lp__"` (which is
#'   not actually a parameter: Stan uses it to record the value of the target
#'   probability density).
#' @param ... Additional arguments to pass to
#'   [rstantools::posterior_interval()].
#'
#' @returns A 3D array: the first index for the parameter, the second for the
#'   percentile, the third for the stanfit object.
#' @examples
#'    eg <- mastiff::stan_example_regression
#'    posterior_intervals(list(eg$posterior_samples, eg$prior_samples), prob =
#'    0.95)
#' @export
posterior_intervals <- function( stanfit_list,
                                 prob,
                                 params_desired = NA,
                                 ... ) {
  check_numeric(prob, lower = 0, upper = 1)
  stopifnot( is.list( stanfit_list ) )
  results_list <- lapply( stanfit_list, function( samples_one_stanfit ) {
    samples_one_stanfit <-
      stanfit_to_matrix( samples_one_stanfit, params_desired )
    rstantools::posterior_interval( samples_one_stanfit,
                                    prob = prob,
                                    ... )
  } )
  simplify2array( results_list )
}

#' Calculates the amount of probability (mass) a parameter has in a range
#'
#' @param stanfit A stanfit object e.g. as returned by [rstan::sampling()]. The
#'   function's name reflects the assumption that this contains samples from the
#'   posterior, but it could contain samples from the prior.
#' @param param The name of the parameter, as a character.
#' @param range The lower and upper boundaries of the range, as a length-2
#'   numeric vector. The boundaries are not considered part of the range (this
#'   should be irrelevant for continuous parameters). The lower boundary can be
#'   -Inf for a range that is unbounded from below; the upper boundary may be
#'   Inf for a range that is unbounded from above.
#'
#' @returns A single number: the amount of probability (mass) that this
#'   parameter has in this range, estimated as the fraction of samples in the
#'   stanfit object for which the parameter is in the range.
#' @examples
#'   eg <- mastiff::stan_example_regression$posterior_samples
#'   posterior_mass_in_range(eg, "m", c(-Inf, 2))
#' @export
posterior_mass_in_range <- function( stanfit, param, range ) {
  stopifnot( class( stanfit )[[ 1 ]] == "stanfit" )
  stopifnot( is.character( param ) )
  stopifnot( length( param ) == 1 )
  stopifnot( is.numeric( range ) )
  stopifnot( length( range ) == 2 )
  stopifnot( range[[1]] <= range[[2]] )
  m <- stanfit_to_matrix( stanfit, params_desired = param )
  mean( m[ , param ] > range[[ 1 ]] & m[ , param ] < range[[ 2 ]] )
}
