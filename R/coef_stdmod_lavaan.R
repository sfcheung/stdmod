#' @title Standardized Moderation Effect in a 'stdmod_lavaan' Class Object
#'
#' @description Return the estiamte of the standardized
#'              moderation effect
#'              in the output of [stdmod_lavaan()].
#'
#' @details It just extract and return the element `stdmod`.
#'
#' @return
#'  A one-element numeric vector
#'
#' @param object The output of [stdmod_lavaan()].
#' @param ...  Optional arguments. Ignored by the function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @examples
#'
#' # Load a test data of 500 cases
#' # It has one predictor (iv), one moderator (mod), two covariates (v1 and v2),
#' # and one dv (dv). All variables continuous.
#' dat <- test_mod1
#' library(lavaan)
#'
#' mod <-
#' "
#' med ~ iv + mod + iv:mod + cov1
#' dv ~ med + cov2
#' "
#' fit <- sem(mod, dat)
#' coef(fit)
#'
#' # Compute the standardized moderation effect
#' out_noboot <- stdmod_lavaan(fit = fit,
#'                             x = "iv",
#'                             y = "med",
#'                             w = "mod",
#'                             x_w = "iv:mod")
#' coef(out_noboot)
#'
#' # Compute the standardized moderation effect and
#' # its confidence interval based on nonparametric bootstrapping
#' set.seed(8479075)
#' system.time(out_boot <- stdmod_lavaan(fit = fit,
#'                                       x = "iv",
#'                                       y = "med",
#'                                       w = "mod",
#'                                       x_w = "iv:mod",
#'                                       boot_ci = TRUE,
#'                                       R = 100))
#' # In real analysis, R should be at least 2000.
#' coef(out_boot)
#'
#' @export


coef.stdmod_lavaan <- function(object, ...) {
    object$stdmod
  }
