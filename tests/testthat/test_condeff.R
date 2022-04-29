skip("WIP")

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

w_mean <- mean(dat$conscientiousness)
w_sd <- sd(dat$conscientiousness)
dat$w_lo <- dat$conscientiousness - (w_mean - 1.25 * w_sd)
dat$w_me <- dat$conscientiousness - w_mean
dat$w_hi <- dat$conscientiousness - (w_mean + 1.25 * w_sd)
lm_out_lo <-  lm(sleep_duration ~ age + gender + emotional_stability*w_lo, dat)
lm_out_me <-  lm(sleep_duration ~ age + gender + emotional_stability*w_me, dat)
lm_out_hi <-  lm(sleep_duration ~ age + gender + emotional_stability*w_hi, dat)
out_ustd_check <- c(coef(lm_out_hi)["emotional_stability"],
                    coef(lm_out_me)["emotional_stability"],
                    coef(lm_out_lo)["emotional_stability"])
out_ustd[, 3]

test_that("Check ustd, mean +/- a SD", {
    expect_equivalent(
        out_ustd_check, out_ustd[, 3]
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
dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender, city = dat$city)
lm_std_wcat3_check <- lm(sleep_duration ~ age + city*emotional_stability + conscientiousness,
                   dat_std)
identical(coef(lm_std_wcat3), coef(lm_std_wcat3_check))

out_ustd_wcat3 <- cond_effect(lm_out_wcat3, x = emotional_stability, w = city)
out_std_wcat3 <- cond_effect(lm_std_wcat3, x = emotional_stability, w = city)
