#'@title Print method for \code{summary.stdmod} class output
#'
#'@description Print method for \code{summary.stdmod} class output
#'
#'@details Print method for \code{summary.stdmod} class output
#'
#'@return
#'  Nothing
#'
#'@param x The output of the class \code{summary.stdmod}.
#'@param ...  Arguments to be passed to \code{summary.lm}.
#'
#'@examples
#' # "To be prepared"
#' @export

print.summary.stdmod <- function(x, ...) {
  cat("\nSelected variable(s) are centered and/or scaled")
  cat("\nVariable(s) centered:", x$centered)
  cat("\nVariable(s) scaled:", x$scaled)
  cat("\n")
  dat_sc <- data.frame(centered_by = x$centered_by,
                       scaled_by   = x$scaled_by)
  print(dat_sc)
  cat("Note:")
  cat("\nCentered by 0 = No centering; Scaled by 1: No scaling.")
  if (!is.null(x$nboot)) {
      cat("\nNonparametric bootstrapping 95% confidence intervals computed.")
      cat("\nNumber of bootstrap samples is", x$nboot)
    }
  cat("\n")
  xlm <- x
  class(xlm) <- "summary.lm"
  print(xlm)
  }