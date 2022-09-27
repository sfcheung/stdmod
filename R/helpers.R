#' @noRd

# Return SD only for numeric vector
sd2 <- function(x) {
    if (!is.numeric(x)) {
        return(NA)
      } else {
        return(stats::sd(x))
      }
  }