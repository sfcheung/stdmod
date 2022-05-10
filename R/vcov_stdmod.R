#'@title The 'vcov' Method for a 'std_selected' Class Object
#'
#'@description Compute the variance-covariance matrix
#' of estimates in the output of [std_selected()] or
#' [std_selected_boot()].
#'
#'@details If bootstrapping was used to form the confidence intervals,
#' users can request the variance-covariance matrix of the bootstrap estimates.
#'
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'@return
#'  A matrix of the variances and covariances of the parameter estimates.
#'
#'@param object The output of [std_selected()] or [std_selected_boot()].
#'@param type The type of variance-covariance matrix. Default is `"lm"`,
#'            returned by the [stats::vcov()] method for the output of
#'            [lm()]. If set to `"boot"`,
#'            the variance-covariance matrix of the bootstrap estimates
#'            is returned.
#'@param ...  Arguments to be passed to [stats::vcov()].
#'
#'@examples
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#' head(dat)
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#'
#' # Standardize all variables except for categorical variables.
#' # Interaction terms are formed after standardization.
#' lm_std <- std_selected(lm_raw, to_scale = ~ .,
#'                                to_center = ~ .)
#'
#' # VCOV of lm output
#' vcov(lm_std)
#'
#' # Standardize all variables as in std_selected above, and compute the
#' # nonparametric bootstrapping percentile confidence intervals.
#' lm_std_boot <- std_selected_boot(lm_raw,
#'                                  to_scale = ~ .,
#'                                  to_center = ~ .,
#'                                  conf = .95,
#'                                  nboot = 100)
#' # In real analysis, nboot should be at least 2000.
#'
#' # VCOV of lm output
#' vcov(lm_std_boot)
#'
#' # VCOV of bootstrap estimates
#' vcov(lm_std_boot, type = "boot")
#'
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
