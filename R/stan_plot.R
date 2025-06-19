#' Plots the marginal posteriors of each parameter in a stanfit object
#'
#' @param posterior_samples A stanfit object e.g. as returned by
#'   `rstan::sampling()`. This should contain samples from the posterior.
#' @param prior_samples A stanfit object containing samples from the prior, so
#'   that the prior and posterior distributions are overlaid. The default value
#'   of NA means only the posterior is plotted.
#' @param true_param_values A named numeric vector: the names are those of
#'   parameters, and the values are the true values (known from simulation
#'   truth). Using this argument means the output plot will include vertical
#'   lines at the true values. The default value of NA means no such lines are
#'   included.
#' @param params_desired A character vector of parameter names. Using this
#'   argument means only named parameters will be included in the plot. The
#'   default value of NA means all parameters are included.
#' @param transforms A named list of functions, with the names being parameter
#'   names. Using this argument means each parameter named in this list will be
#'   transformed by applying the associated function to its sampled values
#'   before plotting. e.g. specifying `list(x=log)` would result in the `log`
#'   function being applied to all values of parameter `"x"`. Note that the
#'   names of parameters as they appear in the plot are not updated to reflect
#'   the functions applied, therefore this argument should usually be used
#'   together with the `labels` argument. The default value of NA means no
#'   transforms are applied to any parameter.
#' @param labels A named character vector, with the names being the names of
#'   parameters as they are named within the stanfit object, and the values
#'   being new names to use instead. e.g. if `transforms=list(x=log)` were
#'   specified, `labels(x="log(x)"` would make sense, so that the label of the
#'   plot of the posterior for x reflects the log transformation.
#'
#' @returns A ggplot object.
#' @examples
#' eg <- mastiff::stan_example_regression
#' plot_posterior(eg$posterior_samples)
#' plot_posterior(eg$posterior_samples,
#'                prior_samples = eg$prior_samples,
#'                true_param_values = eg$true_values)
#' @importFrom data.table :=
#' @export
plot_posterior <- function( posterior_samples,
                            prior_samples = NA,
                            true_param_values = NA,
                            params_desired = NA,
                            transforms = NA,
                            labels = NA ) {

  # Check args
  dt_posterior <- stanfit_to_dt( posterior_samples, params_desired )
  params <- names( dt_posterior )
  have_prior <- ! identical(NA, prior_samples )
  have_true_param_values <- ! identical(NA, true_param_values )
  have_params_desired <- ! identical(NA, params_desired )
  have_transforms <- ! identical(NA, transforms )
  have_labels <- ! identical(NA, labels )
  if ( have_prior ) {
    dt_prior <- stanfit_to_dt( prior_samples, params_desired )
    stopifnot( identical( sort( params ),
                          sort( names( dt_prior ) ) ) )
  }
  if ( have_true_param_values ) {
    stopifnot( is.numeric( true_param_values ) )
    unexpected_true_params <- names( true_param_values )[
      ! names( true_param_values ) %in% params ]
    if ( length( unexpected_true_params ) ) {
      warning( paste( "Ignoring the following params which had true values",
                      "specified, but were not found in the posterior samples:",
                      paste( unexpected_true_params, collapse = " " ) ) )
      true_param_values <-
        true_param_values[ names( true_param_values ) %in% params ]
      if ( ! length(true_param_values) ) have_true_param_values <- FALSE
    }
  }
  if ( have_transforms ) {
    stopifnot( is.list ( transforms ) )
    for ( transform in transforms ) stopifnot( is.function( transform ) )
    stopifnot( all( names( transforms ) %in% params ) )
  }
  if ( have_labels ) {
    stopifnot( is.character ( labels ) )
    stopifnot( all( names( labels ) %in% params ) )
  }

  # Bind posterior and prior if desired
  dt_posterior$density_type <- "posterior"
  if ( have_prior ) {
    dt_prior$density_type <- "prior"
    dt <- rbind( dt_posterior, dt_prior )
  } else {
    dt <- dt_posterior
  }

  # Transform if desired
  if ( have_transforms ) {
    for ( param in names( transforms ) ) {
      dt[[ param ]] <- transforms[[ param ]]( dt[[ param ]] )
    }
    if ( have_true_param_values ) {
      for ( param in names( transforms ) ) {
        true_param_values[[ param ]] <-
          transforms[[ param ]]( true_param_values[[ param ]] )
      }
    }
  }

  # Pivot from wide to long.
  dt <- data.table::melt( dt,
                          id.vars = "density_type",
                          variable.name = "param",
                          variable.factor = FALSE )
  if ( have_true_param_values ) {
    dt_true <- data.table::data.table( param = names( true_param_values ),
                                       value = true_param_values )
  }

  # Rename if desired
  # TODO: for a large data.table it would be noticebly faster to rename the
  # params as column names in the original wide data.table, than as values
  # of the single param column after our pivot from wide to long.
  # But doing that robustly (e.g. imagine labels = c(x="y", y="x")) is tricky
  if ( have_labels ) {
    dt_label <- data.table::data.table( param = names( labels ),
                                        label = labels )
    dt[ dt_label, on = 'param', label := label]
    dt[ , param := ifelse( is.na ( label ), param, label ) ]
    if ( have_true_param_values ) {
      dt_true[ dt_label, on = 'param', label := label ]
      dt_true[ , param := ifelse( is.na ( label ), param, label ) ]
    }
  }

  plot <- ggplot2::ggplot( dt ) +
    ggplot2::facet_wrap( ~ param, scales = "free" ) +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian( expand = FALSE )
  if ( have_prior ) {
    plot <- plot +
      ggplot2::geom_histogram( ggplot2::aes( value, fill = density_type ),
                               bins = 30,
                               position = "identity",
                               alpha = 0.6 ) +
      ggplot2::labs( y = "probability density",
                     fill = "distribution" ) +
      ggplot2::scale_fill_brewer( palette = "Set1" )
  } else {
    plot <- plot +
      ggplot2::geom_histogram( ggplot2::aes( value ),
                               bins = 30 ) +
      ggplot2::labs( y = "posterior density" )
  }
  if ( have_true_param_values ) {
    plot <- plot +
      ggplot2::geom_vline( data = dt_true,
                           ggplot2::aes( xintercept = value ) )
  }
  plot
}
