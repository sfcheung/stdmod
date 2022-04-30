
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

dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender)
dat_std$w_lo <- dat_std$conscientiousness - (-1)
dat_std$w_me <- dat_std$conscientiousness
dat_std$w_hi <- dat_std$conscientiousness - (1)
lm_std_lo <-  lm(sleep_duration ~ age + gender + emotional_stability*w_lo, dat_std)
lm_std_me <-  lm(sleep_duration ~ age + gender + emotional_stability*w_me, dat_std)
lm_std_hi <-  lm(sleep_duration ~ age + gender + emotional_stability*w_hi, dat_std)
out_std_check <- c(coef(lm_std_hi)["emotional_stability"],
                    coef(lm_std_me)["emotional_stability"],
                    coef(lm_std_lo)["emotional_stability"])
out_std[, 3]

test_that("Check ustd, mean +/- a SD", {
    expect_equivalent(
        out_ustd_check, out_ustd[, 3]
      )
  })

test_that("Check std, mean +/- a SD", {
    expect_equivalent(
        out_std_check, out_std[, 3]
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

out_ustd_wcat3 <- cond_effect(lm_out_wcat3, x = "emotional_stability", w = "city")
out_std_wcat3 <- cond_effect(lm_std_wcat3, x = "emotional_stability", w = "city")


dat_tmp <- dat
dat_tmp_std <- data.frame(scale(dat_tmp[, 2:5]), age = dat$gender, city = dat$city)
dat_tmp_std$city <- relevel(factor(dat_tmp_std$city), ref = "Alpha")
lm_std_Alpha <-  lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat_tmp_std)
dat_tmp_std$city <- relevel(factor(dat$city), ref = "Beta")
lm_std_Beta <-  lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat_tmp_std)
dat_tmp_std$city <- relevel(factor(dat$city), ref = "Gamma")
lm_std_Gamma <-  lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat_tmp_std)
out_std_wcat3_check <- c(coef(lm_std_Alpha)["emotional_stability"],
                         coef(lm_std_Beta)["emotional_stability"],
                         coef(lm_std_Gamma)["emotional_stability"])
out_std_wcat3[, 3]

test_that("Check ustd, 3 categories", {
    expect_equivalent(
        out_ustd_wcat3[, 3], out_ustd_wcat3_check
      )
  })

test_that("Check std, 3 categories", {
    expect_equivalent(
        out_std_wcat3[, 3], out_std_wcat3_check
      )
  })

