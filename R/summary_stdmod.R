#'@title Summary method for \code{std_selected} class output
#'
#'@description Summary method for \code{std_selected} class output
#'
#'@details Summary method for \code{std_selected} class output
#'
#'@return
#'  An object of class \code{summary.std_selected}, with bootstrapping confidence intervals
#'  added if present.
#'
#'@param object The output of the class \code{std_selected}.
#'@param ...  Arguments to be passed to \code{summary.lm}.
#'
#'@examples
#' # See examples for std_selected.
#' @export

summary.std_selected <- function(object, ...) {
  out <- stats::summary.lm(object, ...)
  out$scaled_terms <- object$scaled_terms
  out$centered_terms <- object$centered_terms
  out$scaled_by <- object$scaled_by
  out$centered_by <- object$centered_by
  out$nboot <- object$nboot
  if (!is.null(object$boot_ci)) {
    out$coefficients <- cbind(out$coefficients, object$boot_ci)
    }
  class(out) <- c("summary.std_selected", class(out))
  out
  }
