library(testthat)
library(stdmod)
library(ggplot2)

dat <- sleep_emo_con
lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, dat)
lm_std <- std_selected(lm_out,
                      to_center = ~ .,
                      to_scale = ~ .)
dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender)
lm_std_check <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness,
                   dat_std)
identical(coef(lm_std), coef(lm_std_check))

p0 <- plotmod_tumble(output = lm_out,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")
p0
p0 + coord_fixed(ratio = (1/.3)*sd(dat$emotional_stability)/sd(dat$sleep_duration))

p0_check <- structure(list(x = c(1.7704117227118, 2.195380906087), xend = c(3.27637190301919, 
3.69434882364273), y = c(6.27828864766516, 7.0325784780084),
    yend = c(6.90278158628822, 7.05128943568469)), class = "data.frame", row.names = c(NA,
-2L))

test_that("Check plotmod_tumble lm", {
  expect_equal(layer_data(p0, 2)[, 2:5],
               p0_check)
  })

p0p <- plotmod_tumble(output = lm_out,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        w_method = "percentile",
        w_percentiles = c(.10, .90))
p0p

p0p_check <- structure(list(x = c(1.76424143533983, 2.22284685095342), xend = c(3.28509189799351, 
3.7306415211396), y = c(6.12936696085203, 7.1029925961128), yend = c(6.87917205410562, 
7.04687146756592)), class = "data.frame", row.names = c(NA, -2L
))

test_that("Check plotmod_tumble lm, percentiles", {
  expect_equal(layer_data(p0p, 2)[, 2:5],
               p0p_check)
  })

p1 <- plotmod_tumble(output = lm_std,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")
p1
p1 + coord_fixed(ratio = (1/.3))

p1_check <- structure(list(x = c(-1.23569602969199, -0.678696388002916), 
    xend = c(0.738139518022633, 1.2859745237813), y = c(-0.351520649673297,
    0.180858188680334), yend = c(0.0892473573312492, 0.19406440930199
    )), class = "data.frame", row.names = c(NA, -2L))

test_that("Check plotmod_tumble lm, std", {
  expect_equal(layer_data(p1, 2)[, 2:5],
               p1_check)
  })


p1p <- plotmod_tumble(output = lm_std,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        w_method = "percentile",
        w_percentiles = c(.10, .90))
p1p

p1p_check <- structure(list(x = c(-1.24378331693482, -0.642697256659458), 
    xend = c(0.749568662219976, 1.33354272470621), y = c(-0.456629788683114,
    0.230556573693766), yend = c(0.0725837158345487, 0.190946201100927
    )), class = "data.frame", row.names = c(NA, -2L))

test_that("Check plotmod_tumble lm, std, percentiles", {
  expect_equal(layer_data(p1p, 2)[, 2:5],
               p1p_check)
  })


# Categorical Moderator

dat <- sleep_emo_con
lm_out_wcat <- lm(sleep_duration ~ age + gender*emotional_stability + conscientiousness, dat)
summary(lm_out_wcat)
lm_std_wcat <- std_selected(lm_out_wcat,
                      to_center = ~ .,
                      to_scale = ~ .)
summary(lm_std_wcat)
dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender)
lm_std_wcat_check <- lm(sleep_duration ~ age + gender*emotional_stability + conscientiousness,
                   dat_std)
identical(coef(lm_std_wcat), coef(lm_std_wcat_check))

p0_wcat <- plotmod_tumble(output = lm_out_wcat,
        x = emotional_stability,
        w = gender,
        x_label = "Emotional Stability",
        w_label = "Gender",
        y_label = "Sleep Duration")
p0_wcat

p0_wcat_check <- structure(list(x = c(1.92857619767485, 2.05891627554369), xend = c(3.45889368184323, 
3.5575543126916), y = c(6.61654530818674, 6.59512147218901), 
    yend = c(7.02064514249012, 6.54487024901611)), class = "data.frame", row.names = c(NA,
-2L))

test_that("Check plotmod_tumble lm, std, percentiles", {
  expect_equal(layer_data(p0_wcat, 2)[, 2:5],
               p0_wcat_check)
  })


p1_wcat <- plotmod_tumble(output = lm_std_wcat,
        x = emotional_stability,
        w = gender,
        x_label = "Emotional Stability",
        w_label = "Gender",
        y_label = "Sleep Duration")
p1_wcat

p1_wcat_check <- structure(list(x = c(-1.02839263140163, -0.857558181491475), 
    xend = c(0.977367606511294, 1.10668036183842), y = c(-0.112778616107886,
    -0.12789959011911), yend = c(0.172435623446994, -0.163366975124769
    )), class = "data.frame", row.names = c(NA, -2L))

test_that("Check plotmod_tumble lm, std, percentiles", {
  expect_equal(layer_data(p1_wcat, 2)[, 2:5],
               p1_wcat_check)
  })


# Categorical Moderator: 3 Groups

dat <- sleep_emo_con
set.seed(4807859)
dat$city <- sample(c("Alpha", "Beta", "Gamma"), nrow(dat), replace = TRUE)
lm_out_wcat3 <- lm(sleep_duration ~ age + city*emotional_stability + conscientiousness, dat)
summary(lm_out_wcat3)
lm_std_wcat3 <- std_selected(lm_out_wcat3,
                      to_center = ~ .,
                      to_scale = ~ .)
summary(lm_std_wcat3)
dat_std <- data.frame(scale(dat[, 2:5]), gender = dat$gender, city = dat$city)
lm_std_wcat3_check <- lm(sleep_duration ~ age + city*emotional_stability + conscientiousness,
                   dat_std)
identical(coef(lm_std_wcat3), coef(lm_std_wcat3_check))

p0_wcat3 <- plotmod_tumble(output = lm_out_wcat3,
        x = emotional_stability,
        w = city,
        x_label = "Emotional Stability",
        w_label = "City",
        y_label = "Sleep Duration")
p0_wcat3


p0_wcat_check <- structure(list(x = c(1.96606187963417, 1.89124587472203, 1.99826986566196
), xend = c(3.43643812036583, 3.41858071487335, 3.57179001457756
), y = c(6.94671722231592, 6.50466744365998, 6.40981328362209
), yend = c(7.08575348944406, 6.81942773846245, 6.91994451844113
)), class = "data.frame", row.names = c(NA, -3L))

test_that("Check plotmod_tumble lm, std, percentiles", {
  expect_equal(layer_data(p0_wcat3, 2)[, 2:5],
               p0_wcat_check)
  })


p1_wcat3 <- plotmod_tumble(output = lm_std_wcat3,
        x = emotional_stability,
        w = city,
        x_label = "Emotional Stability",
        w_label = "City",
        y_label = "Sleep Duration")
p1_wcat3

p1_wcat3_check <- structure(list(x = c(-0.979260806703231, -1.07732082977996, -0.937046365330035
), xend = c(0.947935496640418, 0.924530109026632, 1.1253388461706
), y = c(0.120257191919775, -0.191742172499917, -0.258690373977058
), yend = c(0.218389187613023, 0.0304160926201823, 0.101360979330034
)), class = "data.frame", row.names = c(NA, -3L))

test_that("Check plotmod_tumble lm, std, percentiles", {
  expect_equal(layer_data(p1_wcat3, 2)[, 2:5],
               p1_wcat3_check)
  })
