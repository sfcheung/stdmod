#' @title Summary Method for a 'std_selected' Class Object
#'
#' @description Summarize the results of [std_selected()] or
#'             [std_selected_boot()].
#'
#' @return
#'  An object of class `summary.std_selected`, with
#'  bootstrap confidence intervals added if present in the object.
#'  The object is a list. Its main element `coefficients` is similar to
#'  the
#'  coefficient table in the [summary()] printout of [lm()].
#'  This object is for printing summary information of the results
#'  from [std_selected()] or [std_selected_boot()].
#'
#' @param object The output of [std_selected()] or [std_selected_boot()].
#' @param ...  Additional arguments. Ignored by this function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#'
#' # Standardize all variables except for categorical variables.
#' # Interaction terms are formed after standardization.
#' lm_std <- std_selected(lm_raw, to_scale = ~ .,
#'                                to_center = ~ .)
#' summary(lm_std)
#'
#' # With bootstrapping
#' # nboot = 100 just for illustration. nboot >= 2000 should be used in read
#' # research.
#' lm_std_boot <- std_selected_boot(lm_raw, to_scale = ~ .,
#'                                          to_center = ~ .,
#'                                          nboot = 100)
#' summary(lm_std_boot)
#'
#' @export
#' @importFrom stats anova

summary.std_selected <- function(object, ...) {
    out <- stats::summary.lm(object, ...)
    out$scaled_terms <- object$scaled_terms
    out$centered_terms <- object$centered_terms
    out$scaled_by <- object$scaled_by
    out$centered_by <- object$centered_by
    out$nboot <- object$nboot
    out$std_selected_call <- object$std_selected_call
    out$std_selected_boot_call <- object$std_selected_boot_call
    if (!is.null(object$boot_ci)) {
      out$coefficients <- cbind(out$coefficients[, 1, drop = FALSE],
                                object$boot_ci,
                                out$coefficients[, -1])
      }
    out$highest_order <- tryCatch(highest_order(object),
                                  error = function(e) NA)
    if (!is.na(out$highest_order)) {
        lm_out <- eval(object$lm_out_call, envir = parent.frame())
        out$highest_order <- lmhelprs::highest_order(lm_out)
        out$f_highest <- lmhelprs::test_highest(lm_out)
      } else {
        out$f_highest <- NA
      }
    class(out) <- c("summary.std_selected", class(out))
    out
  }

#' @noRd
# Adapted from lmhelprs

highest_order <- function(lm_out) {
    terms_x <- stats::terms(lm_out)
    labels_x <- labels(terms_x)
    order_x <- attr(terms_x, "order")
    order_max <- which.max(order_x)
    order_min <- which.min(order_x)
    max_n <- sum(order_x == max(order_x))
    if ((order_max == order_min) ||
        (max_n != 1)) {
        stop("No unique highest order term.")
      }
    labels_x[order_max]
  }
