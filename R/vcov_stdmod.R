#'@title vcov method for \code{std_selected} class output
#'
#'@description Compute the variance-covariance matrix
#' of estimates in a \code{std_selected} class output
#'
#'@details If bootstrapping is used to form the confidence interval,
#' users can request the variance-covariance matrix of the bootstrap estimates.
#'
#'@return
#'  A matrix of the variances and covariances.
#'
#'@param object The output of the class \code{std_selected}.
#'@param type The type of variance-covariance matrix. Default is "lm",
#'            returned by the [vcov] method of [lm]. If set to "boot",
#'            the variance-covariance matrix of the bootstrap estimates
#'            is returned.
#'@param ...  Arguments to be passed to \code{summary.lm}.
#'
#'@examples
#' # See examples for std_selected.
#' @export


vcov.std_selected <- function(object, type = "lm", ...) {
    if (!(type %in% c("lm", "boot"))) {
        stop("type must be either lm or boot.")
      }
    if (type == "boot") {
        if (is.null(object$boot_est)) {
            stop("Bootstrap estimates not available in the object.")
          }
        out <- stats::cov(object$boot_est)
        return(out)
      }
    if (type == "lm") {
        NextMethod()
      }
  }
