library(testthat)
library(stdmod)

context("Check standardized moderation effect given a lavaan output")

test_that("stdmod_lavaan", {
  skip_if(!interactive(),
          message = "stdmod_lavaan not tested if not interactive")

  dat <- test_x_1_w_1_v_2_n_500

  # Results based on stdmod

  lm_raw <- lm(dv ~ iv*mod + v1 + v2, dat)
  lm_zall  <- lm(dv ~ iv*mod + v1 + v2, dplyr::mutate(dat, iv = scale(iv)[, 1],
                                                    mod = scale(mod)[, 1],
                                                    dv = scale(dv)[, 1]))
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
  fit <- sem(mod, dat, likelihood = "wishart")
  coef(fit)
  fit_cov_implied <- lavInspect(fit, "implied")
  iv_sd <- sqrt(diag(fit_cov_implied$cov)["iv"])
  mod_sd <- sqrt(diag(fit_cov_implied$cov)["mod"])
  dv_sd <- sqrt(diag(fit_cov_implied$cov)["dv"])
  iv_mod <- coef(fit)["dv~iv_mod"]
  iv_mod_std <- iv_mod * iv_sd * mod_sd / dv_sd
  iv_mod_std

  out_noboot <- stdmod_lavaan(fit = fit, x = "iv",
                                  y = "dv",
                                  w = "mod",
                                  x_w = "iv_mod")
  out_noboot


  set.seed(6589107)
  stdmod_xyw_boot <- stdmod_bootci(lm_raw, x = iv, y = dv, w = mod, 
                                    x_rescale = TRUE, y_rescale = TRUE, w_rescale = TRUE,
                                    nboot = 2000)
  stdmod_xyw_boot$ci
  set.seed(6589107)
  system.time(out_boot <- stdmod_lavaan(fit = fit, x = "iv",
                                  y = "dv",
                                  w = "mod",
                                  x_w = "iv_mod",
                            boot_ci = TRUE, R = 2000))
  out_boot$ci
  round(stdmod_xyw_boot$ci, 5)
  round(out_boot$ci, 5)
  expect_equal(
      stdmod_xyw_boot$ci, out_boot$ci,
      check.attributes = FALSE
    )
  expect_equal(
      stdmod_xyw, iv_mod_std,
      check.attributes = FALSE
    )
  expect_equal(
      stdmod_xyw, out_boot$stdmod,
      check.attributes = FALSE
    )
  })
