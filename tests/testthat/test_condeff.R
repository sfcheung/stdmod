
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

out_ustd_p <- cond_effect(lm_out, x = "emotional_stability", w = "conscientiousness",
                          w_method = "percentile", w_percentiles = c(.20, .40, .90))
out_std_p <- cond_effect(lm_std, x = "emotional_stability", w = "conscientiousness",
                          w_method = "percentile", w_sd_to_percentiles = 1.25)

dat$w_p1 <- dat$conscientiousness - quantile(dat$conscientiousness, .20)
dat$w_p2 <- dat$conscientiousness - quantile(dat$conscientiousness, .40)
dat$w_p3 <- dat$conscientiousness - quantile(dat$conscientiousness, .90)
lm_out_p1 <-  lm(sleep_duration ~ age + gender + emotional_stability*w_p1, dat)
lm_out_p2 <-  lm(sleep_duration ~ age + gender + emotional_stability*w_p2, dat)
lm_out_p3 <-  lm(sleep_duration ~ age + gender + emotional_stability*w_p3, dat)
out_ustd_p_check <- c(coef(lm_out_p3)["emotional_stability"],
                    coef(lm_out_p2)["emotional_stability"],
                    coef(lm_out_p1)["emotional_stability"])
out_ustd_p[, 3]

dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender)
dat_std$w_p1 <- dat_std$conscientiousness - quantile(dat_std$conscientiousness, stats::pnorm(-1.25))
dat_std$w_p2 <- dat_std$conscientiousness - quantile(dat_std$conscientiousness, stats::pnorm(0))
dat_std$w_p3 <- dat_std$conscientiousness - quantile(dat_std$conscientiousness, stats::pnorm(1.25))
lm_std_p1 <-  lm(sleep_duration ~ age + gender + emotional_stability*w_p1, dat_std)
lm_std_p2 <-  lm(sleep_duration ~ age + gender + emotional_stability*w_p2, dat_std)
lm_std_p3 <-  lm(sleep_duration ~ age + gender + emotional_stability*w_p3, dat_std)
out_std_p_check <- c(coef(lm_std_p3)["emotional_stability"],
                    coef(lm_std_p2)["emotional_stability"],
                    coef(lm_std_p1)["emotional_stability"])
out_std_p[, 3]


test_that("Check ustd, percentile", {
    expect_equivalent(
        out_ustd_p[, 3], out_ustd_p_check
      )
  })

test_that("Check ustd, percentile", {
    expect_equivalent(
        out_std_p[, 3], out_std_p_check
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
dat_tmp$city <- relevel(factor(dat_tmp$city), ref = "Alpha")
lm_out_Alpha <-  lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat_tmp)
dat_tmp$city <- relevel(factor(dat_tmp$city), ref = "Beta")
lm_out_Beta <-  lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat_tmp)
dat_tmp$city <- relevel(factor(dat_tmp$city), ref = "Gamma")
lm_out_Gamma <-  lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat_tmp)
out_ustd_wcat3_check <- c(coef(lm_out_Alpha)["emotional_stability"],
                         coef(lm_out_Beta)["emotional_stability"],
                         coef(lm_out_Gamma)["emotional_stability"])
out_ustd_wcat3[, 3]


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

# Test cond_effect_boot with do_boot = FALSE

out_nb_ustd <- cond_effect_boot(lm_out, x = "emotional_stability", w = "conscientiousness",
                        w_from_mean_in_sd = 1.25, do_boot = FALSE)
out_nb_std <- cond_effect_boot(lm_std, x = "emotional_stability", w = "conscientiousness", do_boot = FALSE)
out_nb_ustd_p <- cond_effect_boot(lm_out, x = "emotional_stability", w = "conscientiousness",
                          w_method = "percentile", w_percentiles = c(.20, .40, .90), do_boot = FALSE)
out_nb_std_p <- cond_effect_boot(lm_std, x = "emotional_stability", w = "conscientiousness",
                          w_method = "percentile", w_sd_to_percentiles = 1.25, do_boot = FALSE)
out_nb_ustd_wcat3 <- cond_effect_boot(lm_out_wcat3, x = "emotional_stability", w = "city", do_boot = FALSE)
out_nb_std_wcat3 <- cond_effect_boot(lm_std_wcat3, x = "emotional_stability", w = "city", do_boot = FALSE)


test_that("cond_effect_boot with do_boot = FALSE", {
    expect_equivalent(
        out_nb_ustd, out_ustd
      )
    expect_equivalent(
        out_nb_std, out_std
      )
    expect_equivalent(
        out_nb_ustd_p, out_ustd_p
      )
    expect_equivalent(
        out_nb_std_p, out_std_p
      )
    expect_equivalent(
        out_nb_ustd_wcat3, out_ustd_wcat3
      )
    expect_equivalent(
        out_nb_std_wcat3, out_std_wcat3
      )
  })
