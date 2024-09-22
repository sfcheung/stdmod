
library(testthat)
library(stdmod)
library(boot)

dat <- sleep_emo_con
lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, dat)
lm_std <- std_selected(lm_out,
                      to_center = ~ .,
                      to_scale = ~ .)
out_ustd <- cond_effect(lm_out, x = "emotional_stability", w = "conscientiousness",
                        w_from_mean_in_sd = 1.25)
out_std <- cond_effect(lm_std, x = "emotional_stability", w = "conscientiousness")

set.seed(875415)
boot_out_ustd <- cond_effect_boot(lm_out, x = "emotional_stability", w = "conscientiousness",
                        w_from_mean_in_sd = 1.25,
                        nboot = 50)
tmpfct <- function(d, i, lm_out) {
    if (missing(i)) {
        d_i <- d
      } else {
        d_i <- d[i, ]
      }
    lm_out_i <- update(lm_out, data = d_i)
    out0 <- cond_effect(lm_out_i, x = "emotional_stability", w = "conscientiousness",
                          w_from_mean_in_sd = 1.25)
    out0[, 3]
  }
tmpfct(dat, lm_out = lm_out)
set.seed(875415)
boot_out_ustd_check <- boot(dat, tmpfct, R = 50, lm_out = lm_out)

test_that("Check ustd boot est", {
    expect_equal(
        attr(boot_out_ustd, "boot_est"),
        boot_out_ustd_check$t,
        ignore_attr = TRUE
      )
  })

set.seed(875415)
boot_out_std <- cond_effect_boot(lm_std, x = "emotional_stability", w = "conscientiousness",
                              nboot = 50)
tmpfct <- function(d, i, lm_out) {
    if (missing(i)) {
        d_i <- d
      } else {
        d_i <- d[i, ]
      }
    lm_out_i <- update(lm_out, data = d_i)
    lm_std_i <- std_selected(lm_out_i,
                          to_standardize = ~ .)
    out0 <- cond_effect(lm_std_i, x = "emotional_stability", w = "conscientiousness")
    out0[, 3]
  }
tmpfct(dat, lm_out = lm_out)
out_std[, 3]
set.seed(875415)
boot_out_std_check <- boot(dat, tmpfct, R = 50, lm_out = lm_out)

test_that("Check std boot est", {
    expect_equal(
        attr(boot_out_std, "boot_est"),
        boot_out_std_check$t,
        ignore_attr = TRUE
      )
  })

# Work on std_selected_boot

lm_std_b <- std_selected_boot(lm_out,
                            to_center = ~ gender + emotional_stability + sleep_duration,
                            to_scale = ~ age + gender,
                            to_standardize = ~ age + conscientiousness + emotional_stability + sleep_duration,
                            nboot = 50)
set.seed(875415)
boot_out_std_b <- cond_effect_boot(lm_std_b, x = "emotional_stability", w = "conscientiousness",
                              nboot = 50)
boot_out_std_b
boot_out_std

test_that("Check cond_effect_boot on std_selected_boot", {
    expect_equal(
        attr(boot_out_std_b, "boot_est"),
        attr(boot_out_std, "boot_est"),
        ignore_attr = TRUE
      )
    expect_equal(
        as.data.frame(boot_out_std_b),
        as.data.frame(boot_out_std),
        ignore_attr = TRUE
      )
  })
