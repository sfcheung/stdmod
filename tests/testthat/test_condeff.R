skip("WIP")

library(testthat)
library(stdmod)

dat <- sleep_emo_con
lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, dat)
lm_std <- std_selected(lm_out,
                      to_center = ~ .,
                      to_scale = ~ .)
out_ustd <- cond_effect(lm_out, x = "emotional_stability", w = "conscientiousness")
out_std <- cond_effect(lm_std, x = "emotional_stability", w = "conscientiousness")



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
