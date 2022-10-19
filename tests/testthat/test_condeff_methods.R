
library(testthat)
library(stdmod)

dat <- sleep_emo_con
lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, dat)
lm_std <- std_selected(lm_out,
                      to_center = ~ .,
                      to_scale = ~ .)
out_ustd <- cond_effect(lm_out, x = "emotional_stability", w = "conscientiousness",
                        w_from_mean_in_sd = 1.25)
out_std <- cond_effect(lm_std, x = "emotional_stability", w = "conscientiousness")

out_ustd_p <- cond_effect(lm_out, x = "emotional_stability", w = "conscientiousness",
                          w_method = "percentile", w_percentiles = c(.20, .40, .90))
out_std_p <- cond_effect(lm_std, x = "emotional_stability", w = "conscientiousness",
                          w_method = "percentile", w_sd_to_percentiles = 1.25)


test_that("confint.cond_effect", {
    expect_equivalent(
        confint(out_ustd),
        t(sapply(attr(out_ustd, "out_all"), function(x) {confint(x)["emotional_stability", ]}))
      )
    expect_equivalent(
        confint(out_std, level = .90),
        t(sapply(attr(out_std, "out_all"), function(x) {confint(x, level = .90)["emotional_stability", ]}))
      )
    expect_equivalent(
        confint(out_ustd_p, level = .80),
        t(sapply(attr(out_ustd_p, "out_all"), function(x) {confint(x, level = .80)["emotional_stability", ]}))
      )
    expect_equivalent(
        confint(out_std_p, level = .91),
        t(sapply(attr(out_std_p, "out_all"), function(x) {confint(x, level = .91)["emotional_stability", ]}))
      )
  })

test_that("coef.cond_effect", {
    expect_equivalent(
        coef(out_ustd),
        as.data.frame(out_ustd)[, "x's Effect"]
      )
    expect_equivalent(
        coef(out_std),
        as.data.frame(out_std)[, "x's Effect"]
      )
    expect_equivalent(
        coef(out_ustd_p),
        as.data.frame(out_ustd_p)[, "x's Effect"]
      )
    expect_equivalent(
        coef(out_std_p, level = .91),
        as.data.frame(out_std_p)[, "x's Effect"]
      )
  })


# Categorical Moderator


dat <- sleep_emo_con
set.seed(4807859)
dat$city <- sample(c("Alpha", "Beta", "Gamma"), nrow(dat), replace = TRUE)
lm_out_wcat3 <- lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat)
lm_std_wcat3 <- std_selected(lm_out_wcat3,
                      to_center = ~ .,
                      to_scale = ~ .)
out_ustd_wcat3 <- cond_effect(lm_out_wcat3, x = "emotional_stability", w = "city")
out_std_wcat3 <- cond_effect(lm_std_wcat3, x = "emotional_stability", w = "city")

test_that("confint.cond_effect, categorical", {
    expect_equivalent(
        confint(out_ustd_wcat3),
        t(sapply(attr(out_ustd_wcat3, "out_all"), function(x) {confint(x)["emotional_stability", ]}))
      )
    expect_equivalent(
        confint(out_std_wcat3, level = .90),
        t(sapply(attr(out_std_wcat3, "out_all"), function(x) {confint(x, level = .90)["emotional_stability", ]}))
      )
    expect_equivalent(
        confint(out_ustd_wcat3, level = .80),
        t(sapply(attr(out_ustd_wcat3, "out_all"), function(x) {confint(x, level = .80)["emotional_stability", ]}))
      )
    expect_equivalent(
        confint(out_std_wcat3, level = .91),
        t(sapply(attr(out_std_wcat3, "out_all"), function(x) {confint(x, level = .91)["emotional_stability", ]}))
      )
  })

test_that("coef.cond_effect, categorical", {
    expect_equivalent(
        coef(out_ustd_wcat3),
        as.data.frame(out_ustd_wcat3)[, "x's Effect"]
      )
    expect_equivalent(
        coef(out_std_wcat3),
        as.data.frame(out_std_wcat3)[, "x's Effect"]
      )
    expect_equivalent(
        coef(out_ustd_wcat3),
        as.data.frame(out_ustd_wcat3)[, "x's Effect"]
      )
    expect_equivalent(
        coef(out_std_wcat3),
        as.data.frame(out_std_wcat3)[, "x's Effect"]
      )
  })

