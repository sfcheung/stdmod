library(testthat)
library(stdmod)

context("Check standardized moderation effect given a lavaan output")

dat <- test_mod1

# Results based on std_selected

lm_med <- lm(med ~ iv * mod + cov1, dat)
lm_std <- std_selected(lm_med, to_scale = ~ ., to_center = ~ .)
summary(lm_std)$coefficients

# Results based on stdmod_lavaan

library(lavaan)
mod <- 
"
med ~ iv + mod + iv:mod + cov1
dv ~ med + cov2
"
# Use likelihood = "wishart" to replicate results in lm
fit <- sem(mod, dat, likelihood = "wishart")
coef(fit)
fit_cov_implied <- lavInspect(fit, "implied")
iv_sd <- sqrt(diag(fit_cov_implied$cov)["iv"])
mod_sd <- sqrt(diag(fit_cov_implied$cov)["mod"])
med_sd <- sqrt(diag(fit_cov_implied$cov)["med"])
iv_mod <- coef(fit)["med~iv:mod"]
iv_mod_std <- iv_mod * iv_sd * mod_sd / med_sd
iv_mod_std

out_noboot <- stdmod_lavaan(fit = fit, x = "iv",
                            y = "med",
                            w = "mod",
                            x_w = "iv:mod")
out_noboot

set.seed(6589107)
stdmod_xyw_boot <- std_selected_boot(lm_med,
                                      to_scale = ~ .,
                                      to_center = ~ .,
                                      nboot = 100)
stdmod_xyw_boot$boot_ci["iv:mod", ]
set.seed(6589107)
system.time(out_boot <- stdmod_lavaan(fit = fit,
                                      x = "iv",
                                      y = "med",
                                      w = "mod",
                                      x_w = "iv:mod",
                                      boot_ci = TRUE,
                                      R = 100))
out_boot$ci

test_that("stdmod_lavaan", {
  expect_equal(
      out_boot$ci,
      stdmod_xyw_boot$boot_ci["iv:mod", ],
      check.attributes = FALSE
    )
  expect_equal(
      iv_mod_std,
      coef(stdmod_xyw_boot)["iv:mod"],
      check.attributes = FALSE
    )
  })


dat <- test_mod2

# Results based on std_selected

lm_dv <- lm(dv ~ med + mod + med:mod + cov2, dat)
summary(lm_dv)$coefficients
lm_std <- std_selected(lm_dv, to_scale = ~ ., to_center = ~ .)
summary(lm_std)$coefficients

# Results based on stdmod_lavaan

library(lavaan)
mod <-
"
med ~ iv + cov1
dv ~ med + mod + med:mod + cov2
"
# Use likelihood = "wishart" to replicate results in lm
fit <- sem(mod, dat, likelihood = "wishart")
coef(fit)
fit_cov_implied <- lavInspect(fit, "implied")
med_sd <- sqrt(diag(fit_cov_implied$cov)["med"])
mod_sd <- sqrt(diag(fit_cov_implied$cov)["mod"])
dv_sd <- sqrt(diag(fit_cov_implied$cov)["dv"])
med_mod <- coef(fit)["dv~med:mod"]
med_mod_std <- med_mod * med_sd * mod_sd / dv_sd
med_mod_std

# dv_sd != sd(dat$dv) because the dv part is not saturated

out_noboot <- stdmod_lavaan(fit = fit, x = "med",
                            y = "dv",
                            w = "mod",
                            x_w = "med:mod")
out_noboot$stdmod

tmpfct <- function(d, i) {
    fit_i <- update(fit, data = d[i, ])
    fit_cov_implied <- lavInspect(fit_i, "implied")
    med_sd <- sqrt(diag(fit_cov_implied$cov)["med"])
    mod_sd <- sqrt(diag(fit_cov_implied$cov)["mod"])
    dv_sd <- sqrt(diag(fit_cov_implied$cov)["dv"])
    med_mod <- coef(fit_i)["dv~med:mod"]
    med_mod_std <- med_mod * med_sd * mod_sd / dv_sd
    med_mod_std
  }
set.seed(6589107)
stdmod_xyw_boot <- boot::boot(dat, tmpfct, R = 100)
set.seed(6589107)
system.time(out_boot <- stdmod_lavaan(fit = fit,
                                      x = "med",
                                      y = "dv",
                                      w = "mod",
                                      x_w = "med:mod",
                                      boot_ci = TRUE,
                                      R = 100))
out_boot$boot_out$t

test_that("stdmod_lavaan", {
  expect_equal(
      out_boot$boot_out$t,
      stdmod_xyw_boot$t,
      check.attributes = FALSE
    )
  })
