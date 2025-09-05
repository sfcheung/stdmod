
library(testthat)
library(stdmod)

test_that("Number of interaction terms", {

dat <- sleep_emo_con
lm_out <- lm(sleep_duration ~ age + emotional_stability*gender + gender*conscientiousness, dat)
# chk <- sum(grepl(":", labels(terms(lm_out)), fixed = TRUE))

expect_true(n_inter(lm_out) > 1)

expect_error(cond_effect(lm_out, x = "emotional_stability", w = "gender"))

lm_out <- lm(sleep_duration ~ age + emotional_stability*gender*conscientiousness, dat)
# chk <- sum(grepl(":", labels(terms(lm_out)), fixed = TRUE))

expect_true(n_inter(lm_out) > 1)

lm_out <- lm(sleep_duration ~ age + emotional_stability*gender, dat)

expect_true(n_inter(lm_out) == 1)

set.seed(234)
dat$gp <- sample(c("gp1", "gp2", "gp3"), nrow(dat), replace = TRUE)

lm_out <- lm(sleep_duration ~ age + emotional_stability*gp, dat)

expect_true(n_inter(lm_out) == 1)

})
