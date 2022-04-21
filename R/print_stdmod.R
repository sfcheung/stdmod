#' @title Print Basic Information of a 'std_selected' Class Object
#'
#' @description Provide information of centering and scaling, along with
#'  basic model information printed by [print.lm()].
#'
#' @return
#'  Nothing
#'
#' @param x The output of [std_selected()] or [std_selected_boot()].
#' @param ...  Arguments to be passed to [print.lm()].
#'
#' @examples
#'
#' # Load a sample data set
#' # It has one predictor (iv), one moderator (mod), on covariate (v1),
#' # one categorical covariate (cat1) with three groups, and one dv (dv).
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
#' lm_std
#'
#' # With bootstrapping
#' # nboot = 100 just for illustration. nboot >= 2000 should be used in read
#' # research.
#' lm_std_boot <- std_selected_boot(lm_raw, to_scale = ~ .,
#'                                          to_center = ~ .,
#'                                          nboot = 100)
#' lm_std_boot
#'
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