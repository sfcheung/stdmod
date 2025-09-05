#' @noRd

# Return SD only for numeric vector
sd2 <- function(x) {
    if (!is.numeric(x)) {
        return(NA)
      } else {
        return(stats::sd(x))
      }
  }

#' @noRd

# Find the number of interaction terms
n_inter <- function(lm_out) {
  sum(grepl(":", labels(stats::terms(lm_out)), fixed = TRUE))
}