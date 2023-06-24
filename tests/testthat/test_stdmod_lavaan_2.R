skip("WIP")
library(testthat)
library(stdmod)

stdmod_from_fit2 <- function(fit,
                             est,
                             implied_cov,
                             x,
                             y,
                             w,
                             x_w,
                             standardized_x = TRUE,
                             standardized_y = TRUE,
                             standardized_w = TRUE,
                             full = FALSE
                             ) {
    if (!missing(fit)) {
        est <- lavaan::parameterEstimates(fit)
        implied_cov <- lavaan::lavInspect(fit, "implied")$cov
      }
    sd_x <- ifelse(standardized_x,
                   sqrt(implied_cov[x, x]),
                   1)
    sd_y <- ifelse(standardized_y,
                   sqrt(implied_cov[y, y]),
                   1)
    sd_w <- ifelse(standardized_w,
                   sqrt(implied_cov[w, w]),
                   1)
    est_x_w <- est[(est$lhs == y) &
                   (est$rhs == x_w) &
                   (est$op == "~"), "est"]
    out <- est_x_w * sd_x * sd_w / sd_y
    if (full) {
        return(c(out, est_x_w, sd_x, sd_w, sd_y))
      } else {
        return(out)
      }
  }

stdmod_lavaan2 <- function(fit,
                           x = NULL,
                           y = NULL,
                           w = NULL,
                           x_w = NULL,
                           standardized_x = TRUE,
                           standardized_y = TRUE,
                           standardized_w = TRUE,
                           boot_ci = FALSE,
                           conf = 0.95,
                           boot_out = NULL) {
    if (lavaan::lavInspect(fit, "ngroups") > 1) {
        stop("Multiple-group models are not supported.")
      }
    tmp <- try(lavaan::lavInspect(fit, "boot"),
               silent = TRUE)
    if (boot_ci && inherits(tmp, "try-error") &&
        is.null(boot_out)) {
        stop("Bootstrap confidence interval ",
             "requested but either bootstrapping was ",
             "not done when fitting the model or ",
             "boot_out was not set.")
      }
    stdmod0 <- stdmod_from_fit2(fit = fit,
                               x = x,
                               y = y,
                               w = w,
                               x_w = x_w,
                               standardized_x = standardized_x,
                               standardized_y = standardized_y,
                               standardized_w = standardized_w)
    if (boot_ci) {
        if (is.null(boot_out)) {
            boot_out <- manymome::fit2boot_out(fit)
          }
        est_all <- lapply(boot_out, function(x) x$est)
        implied_cov_all <- lapply(boot_out, function(x) x$implied$cov)
        stdmod_all <- mapply(stdmod_from_fit2,
                             est = est_all,
                             implied_cov = implied_cov_all,
                             MoreArgs = list(x = x,
                                             y = y,
                                             w = w,
                                             x_w = x_w,
                                             standardized_x = standardized_x,
                                             standardized_y = standardized_y,
                                             standardized_w = standardized_w))
        boot_tmp <- list(t0 = stdmod0,
                         t = matrix(stdmod_all, ncol = 1),
                         R = length(stdmod_all))
        stdmod_ci <- boot::boot.ci(boot_tmp,
                            type = "perc",
                            conf = conf)$percent[4:5]
        names(stdmod_ci) <- c("lower_bound", "upper_bound")
      } else {
        stdmod_ci <- NA
        boot_tmp <- NA
      }
    out <- list(stdmod = stdmod0,
                ci     = stdmod_ci,
                boot_out = boot_tmp)
    out$call <- match.call()
    out$fit <- fit
    class(out) <- c("stdmod_lavaan", class(out))
    return(out)
  }

test_that("stdmod_lavaan2", {
  # skip_if(!interactive(),
  #         message = "stdmod_lavaan not tested if not interactive")

  dat <- test_x_1_w_1_v_2_n_500

  transform0 <- function(data, vars) {
      for (x in vars) {
          data[x] <- scale(data[[x]])[, 1]
        }
      data
    }

  # Results based on stdmod
  lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
  lm_zall  <- lm(dv ~ iv*mod + v1 + v2, transform0(dat, c("iv", "mod", "dv")))
  stdmod_xyw <- stdmod(lm_raw, x = iv, y = dv, w = mod,
                      x_rescale = TRUE, y_rescale = TRUE, w_rescale = TRUE)
  stdmod_xyw
  coef(lm_zall)["iv:mod"]

  # Results based on stdmod_lavaan

  library(lavaan)
  # Scale down some variables to facilitate optimization in lavaan.
  dat$dv <- (dat$dv - 6500)/1100
  dat$mod <- (dat$mod - 95)/5
  dat$iv <- (dat$iv - 10)
  dat$iv_mod <- dat$iv * dat$mod

  mod <-
  "
  dv ~ iv + mod + mod + v1 + v2 + iv_mod
  "
  # Use likelihood = "wishart" to replicate results in lm
  # iseed results are different from set.seed() results
  # Results are not expected to be identical
  # because boot::boot() and lavaan::lavaan()
  # draw samples differently.
  suppressWarnings(fit <- sem(mod, dat, likelihood = "wishart", fixed.x = FALSE,
             se = "boot",
             bootstrap = 150,
             iseed = 82526))
  fit_noboot <- sem(mod, dat, likelihood = "wishart", fixed.x = FALSE)
  fit_boot_out <- manymome::do_boot(fit, R = 150,
                                    seed = 82526,
                                    parallel = FALSE)

  out_noboot <- stdmod_lavaan2(fit = fit, x = "iv",
                                  y = "dv",
                                  w = "mod",
                                  x_w = "iv_mod")
  out_noboot
  set.seed(82526)
  stdmod_xyw_boot <- stdmod_boot(lm_raw, x = iv, y = dv, w = mod,
                                    x_rescale = TRUE, y_rescale = TRUE, w_rescale = TRUE,
                                    nboot = 150)
  stdmod_xyw_boot$ci
  out_boot <- stdmod_lavaan2(fit = fit, x = "iv",
                                  y = "dv",
                                  w = "mod",
                                  x_w = "iv_mod",
                            boot_ci = TRUE)
  out_boot$ci
  out_boot_user <- stdmod_lavaan2(fit = fit_noboot, x = "iv",
                                  y = "dv",
                                  w = "mod",
                                  x_w = "iv_mod",
                            boot_ci = TRUE,
                            boot_out = fit_boot_out)
  out_boot_user$ci
  expect_equal(
      out_boot$ci, out_boot_user$ci,
      check.attributes = FALSE
    )
  expect_equal(
      round(stdmod_xyw_boot$ci, 2),
      round(out_boot$ci, 2),
      check.attributes = FALSE
    )
  expect_equal(
      stdmod_xyw, out_boot$stdmod,
      check.attributes = FALSE
    )
  })
