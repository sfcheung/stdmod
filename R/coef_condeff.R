#' @title Conditional Effect in a 'cond_effect'-Class Object
#'
#' @description Return the estimates of the conditional
#'              effects
#'              in the output of
#'              [cond_effect()] or [cond_effect_boot()].
#'
#' @details It just extracts and returns the column of
#'  conditional effects in a `cond_effect`-class object.
#'
#' @return
#'  A numeric vector: The estimates of the conditional
#'  effects in a `cond_effect`-class object.
#'
#' @param object The output of [cond_effect()] or [cond_effect_boot()].
#' @param ...  Optional arguments. Ignored by the function.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
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
#' out <- cond_effect(lm_raw, x = iv, w = mod)
#' out
#' coef(out)
#'
#' lm_std <- std_selected(lm_raw, to_scale = ~ iv + mod, to_center = ~ iv + mod)
#' out <- cond_effect(lm_std, x = iv, w = mod)
#' out
#' coef(out)
#'
#' # Categorical moderator
#' lm_cat <- lm(dv ~ iv*cat1 + v1, dat)
#' summary(lm_cat)
#' out <- cond_effect(lm_cat, x = iv, w = cat1)
#' out
#' coef(out)
#'
#' @export

coef.cond_effect <- function(object, ...) {
    out <- as.data.frame(object)[, "x's Effect"]
    names(out) <- object$Level
    out
  }
