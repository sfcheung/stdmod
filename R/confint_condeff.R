#' @title Confidence Intervals for a 'cond_effect' Class Object
#'
#' @description Return the confidence intervals of estimates
#'              conditional effect in the output of
#'              [cond_effect()] or [cond_effect_boot()].
#'
#' @details If bootstrapping is used to form the confidence interval by
#' [cond_effect_boot()],
#' users can request the percentile confidence intervals of
#' the bootstrap estimates. This method does not do the bootstrapping itself.
#'
#' @return
#'  A matrix of the confidence intervals.
#'
#' @param object The output of [cond_effect()] or [cond_effect_boot()].
#' @param parm Ignored by this function. The confidence intervals for all
#'             available levels will be returned.
#' @param level The level of confidence. For the confidence intervals returned
#'             by [lm()], default is .95, i.e., 95%. For the bootstrap
#'             percentile confidence
#'             intervals, default is the level used in calling
#'             [cond_effect_boot()].
#' @param type The type of the confidence intervals. If est to `"lm"`,
#'            returns the confidence interval given by the [confint()] method
#'            of [lm()].
#'            If set to `"boot"`, the bootstrap percentile confidence intervals
#'            are returned.
#'             Default is `"boot"` if bootstrap estimates are stored in
#'             `object`, and `"lm"` if bootstrap estimates are not stored.
#' @param ...  Additional arguments. Ignored.
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
#' print(out, t_ci = TRUE)
#' confint(out)
#'
#' lm_std <- std_selected(lm_raw, to_scale = ~ iv + mod, to_center = ~ iv + mod)
#' out <- cond_effect(lm_std, x = iv, w = mod)
#' print(out, t_ci = TRUE)
#' confint(out)
#'
#' # Categorical moderator
#' lm_cat <- lm(dv ~ iv*cat1 + v1, dat)
#' summary(lm_cat)
#' out <- cond_effect(lm_cat, x = iv, w = cat1)
#' print(out, t_ci = TRUE)
#' confint(out)
#'
#' @export


confint.cond_effect <- function(object, parm, level = .95, type, ...) {
    if (!missing(type)) {
        if (!(type %in% c("lm", "boot"))) {
            stop("type must be either lm or boot.")
          }
      } else {
        if (is.null(attr(object, "cond_effect_boot_call"))) {
            type <- "lm"
          } else {
            type <- "boot"
          }
      }
    if (type == "boot") {
        if (is.null(attr(object, "boot_est"))) {
            stop("Bootstrap estimates not available in the object.")
          }
        # if ((level != attr(object, "conf")) && is.null(object$boot_out)) {
        #     stop(paste("level is different form conf in",
        #                "std_selected_boot() but full_output is FALSE."))
        #   }
        if (isTRUE(level == attr(object, "conf"))) {
            out <- attr(object,"boot_ci")
            out_all <- attr(object, "out_all")
            tmp <- stats::confint(out_all[[1]], level = level)
            colnames(out) <- colnames(tmp)
            rownames(out) <- object$Level
            return(out)
          } else {
            boot_est <- attr(object, "boot_est")
            # To be replaced by coef():
            est_org <- as.data.frame(object)[, "x's Effect"]
            boot_tmp <- list(t0 = est_org,
                              t = boot_est,
                              R = attr(object, "nboot"))
            # Adapted from boot
            boot_ci <- sapply(seq_along(est_org), function(x) {
                                  if (all(abs(boot_est[, x] -
                                              mean(boot_est[, x], na.rm = TRUE)) <
                                              1e-8) ||
                                      all(is.na(boot_est[, x]))) {
                                      return(c(NA, NA))
                                    }
                                  boot::boot.ci(boot_tmp,
                                        index = x,
                                        type = "perc",
                                        conf = level)$percent[4:5]
                                })
            out <- t(boot_ci)
            out_all <- attr(object, "out_all")
            tmp <- stats::confint(out_all[[1]], level = level)
            colnames(out) <- colnames(tmp)
            rownames(out) <- object$Level
            return(out)
          }
      }
    if (type == "lm") {
        iv <- attr(object, "x")
        out_all <- attr(object, "out_all")
        ci_all <- lapply(out_all, stats::confint, level = level)
        out <- t(sapply(ci_all, function(y) y[iv, ]))
        rownames(out) <- object$Level
        return(out)
      }
    return(NULL)
  }
