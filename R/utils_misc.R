# Trivial wrapper for stopifnot(!). Not exported.
stopif <- function(condition) {
  if (! is.logical(condition) && length(condition) == 1 && ! is.na(condition)) {
    stop(paste("stopif function takes one argument that must be a non-missing",
               "logical of length 1"))
  }
  stopifnot(! condition)
}

