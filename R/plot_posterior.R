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
#'   specified, `labels=c(x="log(x))"` would make sense, so that the label of the
#'   plot of the posterior for x reflects the log transformation.
#' @param bins A positive integer for the number of bins, passed to
#'   [ggplot2::geom_histogram()]. The default is 30.
#' @param lower A lower value for the x axis for one or more params. Any sampled
#'   values that are less than `lower` are increased to exactly equal `lower`,
#'   such that the left-most bin of the histogram will occur at `lower` and will
#'   contain the overflow. Use this arg to specify either a single value to be
#'   used for all params, or a named numeric vector whose names are params.
#' @param upper An upper value for the x axis for one or more params. Any
#'   sampled values that are greater than `upper` are decreased to exactly equal
#'   `upper`, such that the right-most bin of the histogram will occur at
#'   `upper` and will contain the overflow. Use this arg to specify either a
#'   single value to be used for all params, or a named numeric vector whose
#'   names are params.
#' @param skip_stanfit_to_dt If this argument is set to `TRUE`, the
#'   `posterior_samples` argument (and the `prior_samples` argument if used)
#'   should be used to provide a datatable of samples instead of a stanfit
#'   object of samples, for example after having already run
#'   [mastiff::stanfit_to_dt()] on the stanfit objects. This allows e.g. manual
#'   renaming of parameters before plotting.
#'
#' @returns A ggplot object.
#' @examples
#' eg <- mastiff::stan_example_regression
#' plot_posterior(eg$posterior_samples)
#' plot_posterior(eg$posterior_samples,
#'                prior_samples = eg$prior_samples,
#'                true_param_values = eg$true_values)
#' @importFrom data.table :=
#' @importFrom data.table set
#' @export
plot_posterior <- function(posterior_samples,
                           prior_samples = NA,
                           true_param_values = NA,
                           params_desired = NA,
                           transforms = NA,
                           labels = NA,
                           lower = NA,
                           upper = NA,
                           skip_stanfit_to_dt = NA,
                           bins = 30) {

  # What options were set to non-defaults
  have_params_desired <- ! identical(NA, params_desired)
  have_prior <- ! identical(NA, prior_samples)
  have_true_param_values <- ! identical(NA, true_param_values)
  have_transforms <- ! identical(NA, transforms)
  have_labels <- ! identical(NA, labels)
  have_lower <- ! identical(NA, lower)
  have_upper <- ! identical(NA, upper)

  check_numeric(bins, lower = 1)

  # Get the posterior samples into a dt with only the desired params
  if (identical(skip_stanfit_to_dt, NA)) {
    skip_stanfit_to_dt <- FALSE
  } else {
    stopifnot(is.logical(skip_stanfit_to_dt))
    stopifnot(! is.na(skip_stanfit_to_dt))
  }
  if (skip_stanfit_to_dt) {
    stopifnot(is.data.frame(posterior_samples))
    dt_posterior <- data.table::copy(posterior_samples)
    data.table::setDT(dt_posterior)
    params_all <- names(dt_posterior) # params including those excluded
    if (have_params_desired) {
      stopifnot(is.character(params_desired))
      stopifnot(length(params_desired) > 0L)
      if (anyDuplicated(params_desired)) {
        warning("Duplicates are present in params_desired. Ignoring them.")
        params_desired <- unique(params_desired)
      }
      for (param in params_desired) {
        if (! param %in% colnames(dt_posterior)) {
          stop(paste("Parameter", param, "not present in posterior_samples"))
        }
      }
      dt_posterior <- dt_posterior[ , params_desired, with = FALSE ]
    }
    params <- names(dt_posterior)
  } else {
    dt_posterior <- stanfit_to_dt(posterior_samples, params_desired)
    params <- names(dt_posterior) # params included
    params_all <- posterior_samples@model_pars # params including those excluded
  }

  # Get the prior samples into a dt with only the desired params
  if (have_prior) {
    if (skip_stanfit_to_dt) {
      stopifnot(is.data.frame(prior_samples))
      dt_prior <- data.table::copy(prior_samples)
      data.table::setDT(dt_prior)
      if (have_params_desired) {
        for (param in params_desired) {
          if (! param %in% colnames(dt_prior)) {
            stop(paste("Parameter", param, "not present in prior_samples"))
          }
        }
        dt_prior <- dt_prior[ , params_desired, with = FALSE ]
      }
      stopifnot(identical(sort(names(dt_posterior)),
                          sort(names(dt_prior))))
    } else {
      stopifnot(identical(sort(posterior_samples@model_pars),
                          sort(prior_samples@model_pars)))
      dt_prior <- stanfit_to_dt(prior_samples, params_desired)
    }
  }

  # Silently ignore any params in true_param_values that are not in params.
  # Noisily ignore any params in true_param_values that are not in params_all.
  if (have_true_param_values) {
    stopifnot(is.numeric(true_param_values))
    stopifnot(! is.null(names(true_param_values)))
    stopifnot(! anyDuplicated(names(true_param_values)))
    noisy_params_to_skip <- names(true_param_values)[
      ! names(true_param_values) %in% params_all ]
    if (length(noisy_params_to_skip)) {
      warning(paste("Ignoring the following params which had true values",
                    "specified, but were not found in the posterior samples:",
                    paste(noisy_params_to_skip, collapse = " ")))
    }
    true_param_values <-
      true_param_values[ names(true_param_values) %in% params ]
    if (! length(true_param_values)) have_true_param_values <- FALSE
  }

  # Silently ignore any params named in lower that are not in params.
  # Noisily ignore any params named in lower that are not in params_all.
  if (have_lower) {
    stopifnot(is.numeric(lower))
    stopifnot(length(lower) > 0)
    stopifnot(!anyNA(lower))
    if (is.null(names(lower))) {
      if (length(lower) > 1) stop(paste(
        "If the 'lower' arg has length > 1, it must be a named numeric vector"
      ))
      have_lower_names <- FALSE
    } else {
      stopifnot(! anyDuplicated(names(lower)))
      noisy_params_to_skip <- names(lower)[! names(lower) %in% params_all ]
      if (length(noisy_params_to_skip)) {
        warning(paste("Ignoring the following params which had lower values",
                      "specified, but were not found in the posterior samples:",
                      paste(noisy_params_to_skip, collapse = " ")))
      }
      lower <- lower[ names(lower) %in% params ]
      if (length(lower)) {
        have_lower_names <- TRUE
      } else {
        have_lower <- FALSE
      }
    }
  }

  # Silently ignore any params named in upper that are not in params.
  # Noisily ignore any params named in upper that are not in params_all.
  if (have_upper) {
    stopifnot(is.numeric(upper))
    stopifnot(length(upper) > 0)
    stopifnot(!anyNA(upper))
    if (is.null(names(upper))) {
      if (length(upper) > 1) stop(paste(
        "If the 'upper' arg has length > 1, it must be a named numeric vector"
      ))
      have_upper_names <- FALSE
    } else {
      stopifnot(! anyDuplicated(names(upper)))
      noisy_params_to_skip <- names(upper)[! names(upper) %in% params_all ]
      if (length(noisy_params_to_skip)) {
        warning(paste("Ignoring the following params which had upper values",
                      "specified, but were not found in the posterior samples:",
                      paste(noisy_params_to_skip, collapse = " ")))
      }
      upper <- upper[ names(upper) %in% params ]
      if (length(upper)) {
        have_upper_names <- TRUE
      } else {
        have_upper <- FALSE
      }
    }
  }

  # Check there are no upper values less than associated lower values
  if (have_lower && have_upper) {
    if (have_lower_names) {
      if (have_upper_names) {
        for (param in names(lower)) {
          if (param %in% names(upper) && upper[[param]] < lower[[param]]) {
            stop(paste("For param", param, "a lower value of", lower[[param]],
                       "and an upper value of", upper[[param]],
                       "were specified. Upper values must be greater than lower values."))
          }
        }
      } else {
        if (any(upper < lower)) {
          stop(paste("At least one of the lower values is greater than the",
                     "single upper value that applies to all params."))
        }
      }
    } else {
      if (have_upper_names) {
        if (any(upper < lower)) {
          stop(paste("At least one of the upper values is less than the",
                     "single lower value that applies to all params."))
        }
      } else {
        if (upper < lower) {
          stop("The upper value is less than the lower value")
        }
      }
    }
  }

  # Check remaining args (after possible exclusion of some params)
  if (have_transforms) {
    stopifnot(is.list (transforms))
    stopifnot(! is.null(names(transforms)))
    stopifnot(! anyDuplicated(names(transforms)))
    for (transform in transforms) {
      if (! is.function(transform)) {
        stop(paste("At least one element in the transforms list is not a function"))
      }
    }
    stopifnot(all(names(transforms) %in% params))
  }
  if (have_labels) {
    stopifnot(is.character (labels))
    stopifnot(! is.null(names(labels)))
    stopifnot(! anyDuplicated(names(labels)))
    stopifnot(all(names(labels) %in% params))
  }

  # Bind posterior and prior if desired
  dt_posterior[, density_type := "posterior" ]
  if (have_prior) {
    dt_prior[, density_type := "prior" ]
    dt <- rbind(dt_posterior, dt_prior)
  } else {
    dt <- dt_posterior
  }

  # Transform if desired
  if (have_transforms) {
    for (param in names(transforms)) {
      dt[[ param ]] <- transforms[[ param ]](dt[[ param ]])
    }
    if (have_true_param_values) {
      for (param in names(transforms)) {
        true_param_values[[ param ]] <-
          transforms[[ param ]](true_param_values[[ param ]])
      }
    }
  }

  # Impose lower and upper values if desired
  if (have_lower) {
    if (is.null(names(lower))) {
      for (j in colnames(dt)) {
        if (j == "density_type") next
        data.table::set(dt, j = j, value = pmax(dt[[j]], lower))
      }
    } else {
      for (j in names(lower)) {
        data.table::set(dt, j = j, value = pmax(dt[[j]], lower[[j]]))
      }
    }
  }
  if (have_upper) {
    if (is.null(names(upper))) {
      for (j in colnames(dt)) {
        if (j == "density_type") next
        data.table::set(dt, j = j, value = pmin(dt[[j]], upper))
      }
    } else {
      for (j in names(upper)) {
        data.table::set(dt, j = j, value = pmin(dt[[j]], upper[[j]]))
      }
    }
  }

  # Pivot from wide to long.
  dt <- data.table::melt(dt,
                         id.vars = "density_type",
                         variable.name = "param",
                         variable.factor = FALSE)
  if (have_true_param_values) {
    dt_true <- data.table::data.table(param = names(true_param_values),
                                      value = true_param_values)
  }

  # Rename if desired
  # TODO: for a large data.table it would be noticebly faster to rename the
  # params as column names in the original wide data.table, than as values
  # of the single param column after our pivot from wide to long.
  # But doing that robustly (e.g. imagine labels = c(x="y", y="x")) is tricky
  if (have_labels) {
    dt_label <- data.table::data.table(param = names(labels),
                                       label = labels)
    dt[ dt_label, on = 'param', label := label]
    dt[ , param := ifelse(is.na (label), param, label) ]
    if (have_true_param_values) {
      dt_true[ dt_label, on = 'param', label := label ]
      dt_true[ , param := ifelse(is.na (label), param, label) ]
    }
  }

  plot <- ggplot2::ggplot(dt) +
    ggplot2::facet_wrap(~ param, scales = "free") +
    ggplot2::theme_classic() +
    ggplot2::coord_cartesian(expand = FALSE)
  if (have_prior) {
    plot <- plot +
      ggplot2::geom_histogram(ggplot2::aes(x = value,
                                           y = ggplot2::after_stat(density),
                                           fill = density_type),
                              bins = bins,
                              position = "identity",
                              alpha = 0.6) +
      ggplot2::labs(y = "probability density",
                    fill = "distribution") +
      ggplot2::scale_fill_brewer(palette = "Set1")
  } else {
    plot <- plot +
      ggplot2::geom_histogram(ggplot2::aes(x = value,
                                           y = ggplot2::after_stat(density)),
                              bins = bins) +
      ggplot2::labs(y = "posterior density")
  }
  if (have_true_param_values) {
    plot <- plot +
      ggplot2::geom_vline(data = dt_true,
                          ggplot2::aes(xintercept = value))
  }
  if (have_lower | have_upper) {
    plot <- plot +
      ggplot2::labs(subtitle = "Edge bins contain overflow")
  }
  plot
}
