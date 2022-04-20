#'@title Print method for \code{std_selected} class output
#'
#'@description Print method for \code{std_selected} class output
#'
#'@return
#'  Nothing
#'
#'@param x The output of the class \code{std_selected}.
#'@param ...  Arguments to be passed to \code{print.lm}.
#'
#'@examples
#' # See examples for std_selected.
#' @export

print.std_selected <- function(x, ...) {
    cat("\n- Variable(s) requested to center:", x$centered_terms)
    cat("\n- Variable(s) requested to scale:", x$scaled_terms)
    if (!is.null(x$nboot)) {
        cat("\n- Nonparametric bootstrapping 95% confidence intervals computed.")
      }
    cat("\n")
    NextMethod()
  }