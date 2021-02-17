#' @title Standardized moderation effect and bootstrapping CI in lavaan
#'
#' @description Compute the standardized moderation effect and bootstrapping CI given a lavaan output
#'
#' @details
#' [stdmod_lavaan()] accepts a [lavaan::lavaan-class] object, the SEM output generated 
#' by [lavaan::lavaan()] and its sibling (e.g, [lavaan::sem()]) and compute 
#' the standardized moderation effect.
#'
#' The standard deviations of the independent variable, moderator, and outcome
#' are computed from the implied covariance matrix returned by [lavaan::lavInspect()].
#'
#' If nonparametric bootstrapping confidence is requested with `R` bootstrapping
#' samples, the model will be fitted `R` times to these samples, and the standardized
#' moderation effect will be computed in each sample. This enasures that all 
#' componenets used in the computation, including the standard deviations, are
#' also computed from the bootstrapping samples.
#' 
#' Note that the compuation can be slow because [lavaan::lavaan()] will be called
#' `R` times.
#'
#' @return
#' A list with these elements:
#'
#'  - `stdmod`: The standardized moderation effect.
#'
#'  - `ci`: The nonparametric bootstrapping confidence interval. `NA` if
#'            confidence interval not requested.
#'
#'  - `boot_out`: The raw output from [boot::boot()]. `NA` if
#'            confidence interval not requested.
#'
#' @param fit The SEM output by [lavaan::lavaan()] or its siblings.
#' @param x The name of the independent variable in the model.
#' @param y The name of the outcome variable in the model.
#' @param w The name of the moderator in the model.
#' @param x_w The name of the product term (x * w) in the model.
#' @param boot_ci Boolean. Whether nonparametric bootstrapping will be
#'                conducted. Default is `FALSE`.
#' @param R The number of nonparametric bootstrapping samples. Default is 100.
#'          Set this to at least 2000 in actual use.
#' @param conf The level of confidence. Default is .95.
#' @param ... Optional arguments to be passed to [boot::boot()].
#'
#' @examples
#' # "To be prepared"
#' @export

stdmod_lavaan <- function(fit, x, y, w, x_w, boot_ci = FALSE, R = 100, conf = 0.95, ...) {
    boot_i <- boot_i_gen(fit = fit, x = x,
                                    y = y,
                                    w = w,
                                    x_w = x_w)
    dat_org <- lavaan::lavInspect(fit, "data")
    if (!boot_ci) {
        stdmod <- boot_i(dat_org)
        boot_out <- NA
        stdmod_ci <- NA
      } else {
        boot_out <- boot::boot(dat_org, boot_i, R = R, ...)
        stdmod <- boot_out$t0
        stdmod_ci <- boot::boot.ci(boot_out, type = "perc", conf = conf)$percent[4:5]
        names(stdmod_ci) <- c("lower_bound", "upper_bound")
      }
    out <- list(stdmod = stdmod,
                ci     = stdmod_ci,
                boot_out = boot_out)
    return(out)
  }

boot_i_gen <- function(fit, x, y, w, x_w) {
  force(fit)
  force(x)
  force(y)
  force(w)
  force(x_w)
  function(dat, i = NULL) {
      if (is.null(i)) {
          dat_i <- dat
        } else {
          dat_i <- dat[i, ]
        }
      fit_i <- tryCatch(lavaan::update(fit, data = dat_i),
                        error = function(e) NA,
                        warning = function(e) NA)
      if (!inherits(fit_i, "lavaan")) {
          return(NA)
        } else {
          stats::coef(fit_i)
          fit_cov_implied <- lavaan::lavInspect(fit_i, "implied")
          x_sd <- sqrt(diag(fit_cov_implied$cov)[x])
          w_sd <- sqrt(diag(fit_cov_implied$cov)[w])
          y_sd <- sqrt(diag(fit_cov_implied$cov)[y])
          x_w_i <- stats::coef(fit_i)[paste0(y, "~", x_w)]
          x_w_std_i <- x_w_i * x_sd * w_sd / y_sd
          return(x_w_std_i)
        }
    }
  }
