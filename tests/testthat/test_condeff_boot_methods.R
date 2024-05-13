
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


set.seed(875415)
boot_out_std <- cond_effect_boot(lm_std, x = "emotional_stability", w = "conscientiousness",
                              nboot = 50)


test_that("confint.cond_effect", {
    expect_equal(
        confint(boot_out_ustd),
        attr(boot_out_ustd, "boot_ci"),
        ignore_attr = TRUE
      )
    expect_true(
        all(confint(boot_out_ustd, level = .90) %*% matrix(c(-1, 1), 2, 1) <
            attr(boot_out_ustd, "boot_ci") %*% matrix(c(-1, 1), 2, 1))
      )
    expect_equal(
        confint(boot_out_std),
        attr(boot_out_std, "boot_ci"),
        ignore_attr = TRUE
      )
    expect_true(
        all(confint(boot_out_std, level = .90) %*% matrix(c(-1, 1), 2, 1) <
            attr(boot_out_std, "boot_ci") %*% matrix(c(-1, 1), 2, 1))
      )
  })

test_that("coef.cond_effect", {
    expect_equal(
        coef(boot_out_ustd),
        as.data.frame(out_ustd)[, "x's Effect"],
        ignore_attr = TRUE
      )
    expect_equal(
        coef(boot_out_std),
        as.data.frame(boot_out_std)[, "x's Effect"],
        ignore_attr = TRUE
      )
  })
