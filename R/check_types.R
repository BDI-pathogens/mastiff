# TODO: document
check_numeric <- function(x,
                          lower = -Inf,
                          lower_inclusive = TRUE,
                          upper = Inf,
                          upper_inclusive = TRUE,
                          allow_missing = FALSE) {

  # First check this function was called correctly
  error_msg <- "check_numeric function called incorrectly:"
  if (! is.numeric(lower)) stop(paste(error_msg, "lower must be numeric"))
  if (! is.numeric(upper)) stop(paste(error_msg, "upper must be numeric"))
  if (! is.logical(lower_inclusive)) stop(paste(error_msg, "lower_inclusive must be logical"))
  if (! is.logical(upper_inclusive)) stop(paste(error_msg, "upper_inclusive must be logical"))
  if (length(lower) != 1) stop(paste(error_msg, "lower must be of length 1"))
  if (length(upper) != 1) stop(paste(error_msg, "upper must be of length 1"))
  if (length(lower_inclusive) != 1) stop(paste(error_msg, "lower_inclusive must be of length 1"))
  if (length(upper_inclusive) != 1) stop(paste(error_msg, "upper_inclusive must be of length 1"))
  if (is.na(lower)) stop(paste(error_msg, "lower must not be missing"))
  if (is.na(upper)) stop(paste(error_msg, "upper must not be missing"))
  if (is.na(lower_inclusive)) stop(paste(error_msg, "lower_inclusive must not be missing"))
  if (is.na(upper_inclusive)) stop(paste(error_msg, "upper_inclusive must not be missing"))
  if (lower > upper) stop(paste(error_msg, "lower must be less than or equal to upper"))

  # Then do the checks the function is designed for
  stopifnot(is.numeric(x))
  stopifnot(length(x) == 1)
  if (! allow_missing) stopifnot(! is.na(x))
  if (lower_inclusive) {
    stopifnot(lower <= x)
  } else {
    stopifnot(lower < x)
  }
  if (upper_inclusive) {
    stopifnot(x <= upper)
  } else {
    stopifnot(x < upper)
  }

  # TODO: check how a missing value behaves with respect to lower and upper constraints

}

# TODO: document
check_logical <- function(x, allow_missing = FALSE) {

  # First check this function was called correctly
  error_msg <- "check_numeric function called incorrectly:"
  if (! is.logical(allow_missing)) stop(paste(error_msg, "allow_missing must be logical"))
  if (length(allow_missing) != 1) stop(paste(error_msg, "allow_missing must be of length 1"))
  if (is.na(allow_missing)) stop(paste(error_msg, "allow_missing must not be missing"))

  # Then do the checks the function is designed for
  stopifnot(is.logical(x))
  stopifnot(length(x) == 1)
  if (! allow_missing) stopifnot(! is.na(x))

}
