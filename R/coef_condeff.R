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
#' # To Do
#'
#' @export

coef.cond_effect <- function(object, ...) {
    out <- as.data.frame(object)[, "x's Effect"]
    names(out) <- object$Level
    out
  }
