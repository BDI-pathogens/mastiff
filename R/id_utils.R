#' Return a length-n vector of unique short strings e.g. for labelling n objects
#'
#' If n <= 26, we return `letters[1:n]`. If 26 < n <= 26^2, we return
#' `c("aa", "ab", "ac", ... "az", "ba", "bb", ...)`. If 26^2 < n <= 26^3, we
#' return `c("aaa", "aab", ...)`
#'
#' @param n a non-negative integer
#'
#' @returns a character vector
#' @export
#'
#' @examples
#' get_n_character_ids(1)
#' get_n_character_ids(5)
#' get_n_character_ids(28)
get_n_character_ids <- function(n) {

  check_numeric(n, lower = 0)
  n <- as.integer(n)

  if (n <= 26) return(letters[seq(1, length.out = n)])

  nchar_needed <- ceiling(log(n, base = 26))
  list_ <- list()
  for (i in 1:nchar_needed) list_[[i]] <- letters
  all_combinations <- expand.grid(list_)
  all_combinations <- all_combinations[, nchar_needed:1]
  all_combinations <-
    tidyr::unite(all_combinations, "new_col", everything(), sep = "")
  all_combinations$new_col[1:n]

}
