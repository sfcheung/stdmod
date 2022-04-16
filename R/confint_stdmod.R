#'@title confint method for \code{std_selected} class output
#'
#'@description Return the confidence intervals of estimates
#'              in a \code{std_selected} class output
#'
#'@details If bootstrapping is used to form the confidence interval,
#' users can request the percentile confidence intervals of
#' the bootstrap estimates.
#'
#'@return
#'  A matrix of the confidence intervals.
#'
#'@param object The output of the class \code{std_selected}.
#'@param parm The parameters for which confidence intervals should be returned.
#'            If missing, the confidence intervals of all parameters will be
#'            returned.
#'@param level The level of confidence. For the confidence intervals returned
#'             by [lm], default is .95. For the bootstrapping confidence
#'             intervals, default is the level used in calling
#'             [std_selected_boot]. If a level different from that in the
#'             original
#'             call is specified, `full_output` needs to be set in the call
#'             to [std_selected_boot] such that the original bootstrap output
#'             is stored.
#'@param type The type of the confidence intervals. Default is "lm",
#'            returned by the [confint] method of [lm]. If set to "boot",
#'            the bootstrap percentile confidence intervals are
#'            returned.
#'@param ...  Arguments to be passed to \code{summary.lm}.
#'
#'@examples
#' # See examples for std_selected.
#' @export


confint.std_selected <- function(object, parm, level = .95, type = "lm", ...) {
    if (!(type %in% c("lm", "boot"))) {
        stop("type must be either lm or boot.")
      }
    if (type == "boot") {
        if (is.null(object$boot_est)) {
            stop("Bootstrap estimates not available in the object.")
          }
        if ((level != object$conf) && is.null(object$boot_out)) {
            stop("level is different form conf in std_selected_boot but full_output is FALSE.")
          }
        if (level == object$conf) {
            out <- object$boot_ci
          } else {
            p <- nrow(object$boot_ci)
            out <- t(sapply(seq_len(p), function(x) {
                        boot::boot.ci(object$boot_out, conf = level,
                                      type = "perc", index = x)$percent[4:5]
                      }))
            rownames(out) <- names(object$boot_out$t0)
          }
        tmp <- object
        class(tmp) <- "lm"
        colnames(out) <- colnames(stats::confint(tmp, level = level))
        if (!missing(parm)) {
            out <- out[rownames(out) %in% parm, , drop = FALSE]
          }
        return(out)
      }
    if (type == "lm") {
        NextMethod()
      }
  }
