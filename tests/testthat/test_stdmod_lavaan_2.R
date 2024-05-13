skip_on_cran()
library(testthat)
library(stdmod)

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
      ignore_attr = TRUE
    )
  expect_equal(
      round(stdmod_xyw_boot$ci, 2),
      round(out_boot$ci, 2),
      ignore_attr = TRUE
    )
  expect_equal(
      stdmod_xyw, out_boot$stdmod,
      ignore_attr = TRUE
    )
  })
