
#' Checks a variable is a single number (and optionally, in a range and not NA)
#'
#' @param x The variable to check.
#' @param lower The lower limit for `x` (default = `-Inf`).
#' @param lower_inclusive Whether the lower limit itself is inclusive, i.e.
#'   whether we require `x >= lower` or `x > lower` (the default is the former).
#' @param upper The upper limit for `x` (default = `Inf`).
#' @param upper_inclusive Whether the upper limit itself is inclusive, i.e.
#'   whether we require `x <= upper` or `x < lower` (the default is the former).
#' @param allow_missing Whether x is allowed to be missing (default = `FALSE`).
#'   If `allow_missing` is `TRUE` we skip the tests comparing `x` to `lower` and
#'   `upper`.
#'
#' @returns `invisible(TRUE)`, if no problems were found; otherwise a
#'   `stopifnot()` is triggered.
#' @export
#'
#' @examples
#' # These are all OK:
#' check_numeric(0)
#' check_numeric(0L)
#' check_numeric(0, lower = 0)
#' check_numeric(0, upper = 0)
#' check_numeric(NA_real_, allow_missing = TRUE)
#' check_numeric(Inf)
#' check_numeric(-Inf)
#' # These return errors:
#' try(check_numeric("foo"))
#' try(check_numeric(1:2))
#' try(check_numeric(NA_real_))
#' try(check_numeric(NA, allow_missing = TRUE)) # NA is logical, not numeric
#' try(check_numeric(0, lower = 1))
#' try(check_numeric(0, lower = 0, lower_inclusive = FALSE))
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
  if (! allow_missing) {
    stopifnot(! is.na(x))
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
  }

  invisible(TRUE)

}


#' Checks a variable is a single logical value (and optionally not missing)
#'
#' @param x The variable to check.
#' @param allow_missing Whether x is allowed to be missing (default = `FALSE`).
#'
#' @returns `invisible(TRUE)`, if no problems were found; otherwise a
#'   `stopifnot()` is triggered.
#' @export
#'
#' @examples
#' # These are all OK:
#' check_logical(TRUE)
#' check_logical(FALSE)
#' check_logical(NA, allow_missing = TRUE)
#' # These return errors:
#' try(check_logical("foo"))
#' try(check_logical(c(TRUE, TRUE)))
#' try(check_logical(NA))
#' try(check_logical(NA_real_, allow_missing = TRUE)) # NA_real_ is not logical
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

  invisible(TRUE)

}
