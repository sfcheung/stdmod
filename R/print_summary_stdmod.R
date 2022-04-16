#'@title Print method for \code{summary.std_selected} class output
#'
#'@description Print method for \code{summary.std_selected} class output
#'
#'@details Print method for \code{summary.std_selected} class output
#'
#'@return
#'  Nothing
#'
#'@param x The output of the class \code{summary.std_selected}.
#'@param ...  Arguments to be passed to \code{summary.lm}.
#'
#'@examples
#' # See examples for std_selected.
#' @export

print.summary.std_selected <- function(x, ...) {
  cat("\nSelected variable(s) are centered and/or scaled")
  cat("\n- Variable(s) centered:", x$centered_terms)
  cat("\n- Variable(s) scaled:", x$scaled_terms)
  cat("\n")
  dat_sc <- data.frame(centered_by = x$centered_by,
                       scaled_by   = x$scaled_by)
  print(dat_sc)
  cat("\nNote:")
  cat("\n- Centered by 0 or NA: No centering\n- Scaled by 1 or NA: No scaling")
  if (!is.null(x$nboot)) {
      cat("\n- Nonparametric bootstrapping 95% confidence intervals computed.")
      cat("\n- The number of bootstrap samples is", x$nboot)
    }
  cat("\n")
  NextMethod()
  if (!is.null(x$nboot)) {
      cat("Note:")
      cat("\n- [CI Lower, CI Upper] are bootstrap percentile confidence intervals.")
      cat("\n- Std. Error are standard errors in the original analysis, not bootstrap SEs.")
      cat("\n")
    }
  }