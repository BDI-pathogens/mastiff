#' Simulate (randomly draw) numbers from a mixture of two normal distributions.
#'
#' @param n A non-negative integer: the number of observations to simulate.
#' @param groups A character vector: a set of categories that differ in
#'   their proportions for the mixture. Specify `character(0)` to have all
#'   observations in the same category, i.e. all observations have the same
#'   probability of coming from one component of the mixture or the other.
#' @param group_frequencies A numeric vector of the same length as
#'   `groups`, each value being the frequency of that group (as a
#'   proportion between 0 and 1).
#' @param mu_0 A number: the smaller of the means of the two normals.
#' @param mu_1 A number: the larger of the means of the two normals.
#' @param sd_0 A non-negative number: the standard deviation of the normal with
#'   mean mu_0.
#' @param sd_1 A non-negative number: the standard deviation of the normal with
#'   mean mu_1.
#' @param p The probability of an observation coming from the normal with mean
#'   mu_1.
#' @param sd_groups A non-negative number: the standard deviation of the
#'   normal variability between regression coefficients for the groups (on a
#'   logit scale).
#'
#' @returns A dataframe with `n` rows, one per observation. The column `y`
#'   contains the values drawn from the normal mixture. The column `d` contains
#'   a logical variable for whether that observation came from the normal
#'   with mean `mu_1`.
#' @importFrom purrr map_dbl
#' @importFrom purrr map_chr
#' @importFrom dplyr if_else
#' @export
#'
#' @examples
#' df <- simulate_mixture_of_two_normals()$data
#' hist(df$y)
#' hist(df[df$d, ]$y)   # the normal with mean mu_1
#' hist(df[! df$d, ]$y) # the normal with mean mu_0
simulate_mixture_of_two_normals <- function(
    n = 500,
    groups = LETTERS[1:5],
    group_frequencies = rep(1 / length(groups), length(groups)),
    mu_0 = 0,
    mu_1 = mu_0 + 3,
    sd_0 = 0.5,
    sd_1 = 1,
    p = 0.5,
    sd_groups = 1
) {

  # Check args
  check_numeric(n, lower = 0)
  check_numeric(mu_0)
  check_numeric(mu_1, lower = mu_0)
  check_numeric(sd_0, lower = 0)
  check_numeric(sd_1, lower = 0)
  check_numeric(p, lower = 0, upper = 1)
  check_numeric(sd_groups, lower = 0)
  stopifnot(is.character(groups))
  stopifnot(! anyDuplicated(groups))
  stopifnot(is.numeric(group_frequencies))
  stopifnot(length(group_frequencies) == length(groups))

  # If we have groups, calculate group-specific p parameters, otherwise all
  # observations use p
  have_groups <- length(groups) > 1
  if (have_groups) {
    stopifnot(all(group_frequencies >= 0))
    stopifnot(sum(group_frequencies) == 1)
    beta_by_group <- stats::rnorm(length(groups),
                                  mean = 0,
                                  sd = sd_groups)
    p_by_group <- logistic(logit(p) + beta_by_group)
    names(p_by_group) <- groups
    df <- data.frame(group = sample(groups,
                                    size = n,
                                    replace = TRUE))
    df$p_group <- purrr::map_dbl(df$group,
                                 function(pred_) p_by_group[[pred_]])
    df$d <- stats::runif(n) < df$p_group
    df$p_group <- NULL
  } else {
    df <- data.frame(d = stats::runif(n) < p)
  }

  df$y <- dplyr::if_else(df$d,
                         stats::rnorm(n, mean = mu_1, sd = sd_1),
                         stats::rnorm(n, mean = mu_0, sd = sd_0))
  params <- c(
    mu_0 = mu_0,
    mu_1 = mu_1,
    sd_0 = sd_0,
    sd_1 = sd_1,
    p = p,
    sd_groups = sd_groups
  )
  if (have_groups) {
    params <- params[names(params) != "p"]
    names(p_by_group) <- paste0("p_for_", groups)
    params <- c(params, p_by_group)
  }

  list(params = params, data = df)

}


#' @importFrom data.table setnames
#' @export
estimate_mixture_of_two_normals <- function(
    y,
    prior_boundaries,
    groups = NA,
    sample_posterior_not_prior = TRUE,
    cores = 1,
    report_stan_progress = FALSE,
    ... ) {

  # Check args
  stopifnot(is.numeric(y))
  n <- length(y)
  stopifnot(is.data.frame(prior_boundaries))
  stopifnot(all(c("param", "lower", "upper") %in% names(prior_boundaries)))
  stopifnot(is.character(prior_boundaries$param))
  stopifnot(is.numeric(prior_boundaries$lower))
  stopifnot(is.numeric(prior_boundaries$upper))
  stopifnot(all(c("mu_0", "mu_1", "sd_0", "sd_1", "sd_groups", "p") %in%
                  prior_boundaries$param))
  stopifnot(all(prior_boundaries$lower < prior_boundaries$upper))
  stopifnot(! anyDuplicated(prior_boundaries$param))
  stopifnot(prior_boundaries[prior_boundaries$param == "sd_0", ]$lower >= 0)
  stopifnot(prior_boundaries[prior_boundaries$param == "sd_1", ]$lower >= 0)
  stopifnot(prior_boundaries[prior_boundaries$param == "sd_groups", ]$lower >= 0)
  stopifnot(prior_boundaries[prior_boundaries$param == "p", ]$lower >= 0)
  stopifnot(prior_boundaries[prior_boundaries$param == "p", ]$upper <= 1)
  check_logical(sample_posterior_not_prior)
  check_logical(report_stan_progress)
  have_groups <- ! identical(groups, NA)

  # Make a model matrix out of the groups vector: if there are m different
  # values of groups, make an n * m matrix where the row for observation i
  # is zeros except for the column corresponding to the group for i.
  if (have_groups) {
    stopifnot(is.character(groups))
    stopifnot(length(groups) == n)
    num_groups <- length(unique(groups))
    stopifnot(num_groups > 1)
    groups_as_ints <- as.factor(groups)
    groups_as_ints_key <- levels(groups_as_ints)
    groups_as_ints <- matrix(as.integer(groups_as_ints), nrow = 1)
    groups_recalculated <-
      purrr::map_chr(groups_as_ints, function(int_) groups_as_ints_key[[int_]])
    stopifnot(isTRUE(all.equal(unname(groups),
                               unname(groups_recalculated))))
    ## Stash the model matrix formulation:
    #x <- model.matrix(~ col_ - 1, data.frame(col_ = groups))
    #colnames(x) <- sub("^col_", "", colnames(x))
    #stopifnot(identical(sort(colnames(x)),
    #                    sort(unique(groups))))
    #which_col_by_row <- apply(x, 1, function(row) which(as.logical(row)))
    #groups_recalculated <-
    #  purrr::map_chr(which_col_by_row, function(col) colnames(x)[[col]])
    #stopifnot(isTRUE(all.equal(unname(groups),
    #                           unname(groups_recalculated))))
  } else {
    groups_as_ints <- matrix(ncol = n, nrow = 0)
    num_groups <- 0
    #x <- matrix(nrow = n, ncol = 0)
  }

  stan_input <- list(y = y,
                     num_groups = num_groups,
                     groups = groups_as_ints)
  for (row in 1:nrow(prior_boundaries)) {
    stan_input[[paste0(prior_boundaries$param[[row]], "_lower")]] <-
      prior_boundaries$lower[[row]]
    stan_input[[paste0(prior_boundaries$param[[row]], "_upper")]] <-
      prior_boundaries$upper[[row]]
  }
  params_to_ignore <- c("p_by_i_log",
                        "p_by_i_log1m",
                        "lp_1",
                        "lp_0",
                        "beta_unscaled_p")
  if (! sample_posterior_not_prior) {
    params_to_ignore <- c(params_to_ignore, "lp_0", "lp_1", "prob_is_1")
  }

  if (report_stan_progress) {
    samples <- rstan::sampling( stanmodels$mixture_of_two_normals,
                                data = stan_input,
                                cores = cores,
                                pars = params_to_ignore,
                                include = FALSE,
                                ... )
  } else {
    capture.output( samples <- rstan::sampling( stanmodels$mixture_of_two_normals,
                                                data = stan_input,
                                                cores = cores,
                                                pars = params_to_ignore,
                                                include = FALSE,
                                                refresh = -1,
                                                ... )  )
  }

  # Rename variables like "p_by_group[1]" to "p_for_ (then the value of
  # the first group character)"
  dt <- stanfit_to_dt(samples)
  if (have_groups) {
    col_names_existing <- paste0("p_by_group[", 1:num_groups, "]")
    stopifnot(all(col_names_existing %in% colnames(dt)))
    col_names_new <- paste0("p_for_", groups_as_ints_key)
    data.table::setnames(dt, col_names_existing, col_names_new)
    data.table::setnames(dt, "sd_groups[1]", "sd_groups")

  }

  dt

}
