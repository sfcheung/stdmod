#' @title Standardized Moderation Effect and Bootstrap CI in 'lavaan'
#'
#' @description Compute the standardized moderation effect in a structural
#'              equation model fitted by [lavaan::lavaan()] and
#'              form its nonparametric bootstrap confidence interval.
#'
#' @details
#' [stdmod_lavaan()] accepts a [lavaan::lavaan-class] object, the
#' structural equation model output returned
#' by [lavaan::lavaan()] and its wrappers (e.g, [lavaan::sem()]) and computes
#' the standardized moderation effect using the formula in the appendix of
#' Cheung, Cheung, Lau, Hui, and Vong (2022).
#'
#' The standard deviations of the independent variable, moderator, and outcome
#' variable are computed from the implied covariance matrix returned by
#' [lavaan::lavInspect()]. Therefore, models fitted to data sets with missing
#' data (e.g., with `missing = "fiml"`) are also supported.
#'
#' If nonparametric bootstrap confidence interval is requested with `R`
#' bootstrap samples, the model will be fitted `R` times to these samples,
#' and the standardized
#' moderation effect will be computed in each sample. This ensures that all
#' components used in the computation, including the standard deviations, are
#' also computed from the bootstrapping samples.
#'
#' Note that the computation can be slow because [lavaan::lavaan()]
#' will be called
#' `R` times.
#'
#' @return
#' A list with these elements:
#'
#'  - `stdmod`: The standardized moderation effect.
#'
#'  - `ci`: The nonparametric bootstrap confidence interval. `NA` if
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
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#'
#' @references
#' Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N.
#' (2022) Improving an old way to measure moderation effect in standardized
#' units. Advance online publication. *Health Psychology*.
#' \doi{10.1037/hea0001188}
#'
#' @examples
#'
#' # Load a test data of 500 cases
#' # It has one predictor (iv), one moderator (mod), two covariates (v1 and v2),
#' # and one dv (dv). All variables continuous.
#' dat <- test_x_1_w_1_v_2_n_500
#' library(lavaan)
#' # Scale down some variables to facilitate optimization in lavaan.
#' dat$dv <- (dat$dv - 6500)/1100
#' dat$mod <- (dat$mod - 95)/5
#' dat$iv <- (dat$iv - 10)
#' # Form the product term for the moderation
#' dat$iv_mod <- dat$iv * dat$mod
#'
#' mod <-
#' "
#' dv ~ iv + mod + mod + v1 + v2 + iv_mod
#' "
#' fit <- sem(mod, dat)
#' coef(fit)
#'
#' # Compute the standardized moderation effect
#' out_noboot <- stdmod_lavaan(fit = fit, x = "iv",
#'                                 y = "dv",
#'                                 w = "mod",
#'                                 x_w = "iv_mod")
#' out_noboot$stdmod
#'
#' # Compute the standardized moderation effect and
#' # its confidence interval based on nonparametric bootstrapping
#' set.seed(8479075)
#' system.time(out_boot <- stdmod_lavaan(fit = fit, x = "iv",
#'                                 y = "dv",
#'                                 w = "mod",
#'                                 x_w = "iv_mod",
#'                           boot_ci = TRUE, R = 50))
#' # In real analysis, R should be at least 2000.
#' out_boot$ci
#'
#' # Use boot.ci to compute the confidence interval using other settings
#' library(boot)
#' boot.ci(out_boot$boot_out, conf = .90, type = "perc")
#'
#' @export

stdmod_lavaan <- function(fit,
                          x,
                          y,
                          w,
                          x_w,
                          boot_ci = FALSE,
                          R = 100,
                          conf = 0.95, ...) {
    boot_i <- boot_i_gen(fit = fit, x = x,
                                    y = y,
                                    w = w,
                                    x_w = x_w)
    dat_org <- lavaan::lavInspect(fit, "data")
    if (!boot_ci) {
        stdmod <- stdmod_from_fit(fit = fit,
                                  x = x,
                                  y = y,
                                  w = w,
                                  x_w = x_w)
        boot_out <- NA
        stdmod_ci <- NA
      } else {
        boot_out <- boot::boot(dat_org, boot_i, R = R, ...)
        stdmod <- boot_out$t0
        stdmod_ci <- boot::boot.ci(boot_out,
                                   type = "perc",
                                   conf = conf)$percent[4:5]
        names(stdmod_ci) <- c("lower_bound", "upper_bound")
      }
    out <- list(stdmod = stdmod,
                ci     = stdmod_ci,
                boot_out = boot_out)
    out$call <- match.call()
    out$fit <- fit
    class(out) <- c("stdmod_lavaan", class(out))
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
      fit_i <- tryCatch(lavaan::update(fit,
                                       data = dat_i,
                                       se = "none",
                                       h1 = FALSE,
                                       baseline = FALSE,
                                       test = "standard"
                                       ),
                        error = function(e) NA,
                        warning = function(e) NA)
      if (!inherits(fit_i, "lavaan")) {
          return(NA)
        } else {
          x_w_std_i <- stdmod_from_fit(fit = fit_i,
                                       x = x,
                                       y = y,
                                       w = w,
                                       x_w = x_w)
          return(x_w_std_i)
        }
    }
  }

stdmod_from_fit <- function(fit, x, y, w, x_w) {
    fit_cov_implied <- lavaan::lavInspect(fit, "implied")
    x_sd <- sqrt(diag(fit_cov_implied$cov)[x])
    w_sd <- sqrt(diag(fit_cov_implied$cov)[w])
    y_sd <- sqrt(diag(fit_cov_implied$cov)[y])
    x_w_i <- lavaan::coef(fit)[paste0(y, "~", x_w)]
    x_w_std_i <- x_w_i * x_sd * w_sd / y_sd
    return(x_w_std_i)
  }