library(testthat)
library(stdmod)

dat <- sleep_emo_con
lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, dat)
lm_std <- std_selected(lm_out,
                      to_center = ~ .,
                      to_scale = ~ .)
out_ustd <- cond_effect(lm_out, x = "emotional_stability", w = "conscientiousness",
                        w_values = c(20, 3, 54, -4))
out_std <- cond_effect(lm_std, x = "emotional_stability", w = "conscientiousness",
                       w_values = c(-1.5, 23))

dat$w_a <- dat$conscientiousness - 54
dat$w_b <- dat$conscientiousness - 20
dat$w_c <- dat$conscientiousness - 3
dat$w_d <- dat$conscientiousness - -4
lm_out_a <-  lm(sleep_duration ~ age + gender + emotional_stability*w_a, dat)
lm_out_b <-  lm(sleep_duration ~ age + gender + emotional_stability*w_b, dat)
lm_out_c <-  lm(sleep_duration ~ age + gender + emotional_stability*w_c, dat)
lm_out_d <-  lm(sleep_duration ~ age + gender + emotional_stability*w_d, dat)
out_ustd_check <- c(coef(lm_out_a)["emotional_stability"],
                    coef(lm_out_b)["emotional_stability"],
                    coef(lm_out_c)["emotional_stability"],
                    coef(lm_out_d)["emotional_stability"])
out_ustd[, 3]

dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender)
dat_std$w_a <- dat_std$conscientiousness - 23
dat_std$w_b <- dat_std$conscientiousness - (-1.5)
lm_std_a <-  lm(sleep_duration ~ age + gender + emotional_stability*w_a, dat_std)
lm_std_b <-  lm(sleep_duration ~ age + gender + emotional_stability*w_b, dat_std)
out_std_check <- c(coef(lm_std_a)["emotional_stability"],
                   coef(lm_std_b)["emotional_stability"])
out_std[, 3]

test_that("Check ustd", {
    expect_equivalent(
        out_ustd_check, out_ustd[, 3]
      )
  })

test_that("Check std", {
    expect_equivalent(
        out_std_check, out_std[, 3]
      )
  })

# Test cond_effect_boot with do_boot = FALSE

out_nb_ustd <- cond_effect_boot(lm_out, x = "emotional_stability", w = "conscientiousness",
                                w_values = c(20, 3, 54, -4), do_boot = FALSE)
out_nb_std <- cond_effect_boot(lm_std, x = "emotional_stability", w = "conscientiousness",,
                               w_values = c(-1.5, 23), do_boot = FALSE)

test_that("cond_effect_boot with do_boot = FALSE", {
    expect_equivalent(
        out_nb_ustd, out_ustd
      )
    expect_equivalent(
        out_nb_std, out_std
      )
  })
