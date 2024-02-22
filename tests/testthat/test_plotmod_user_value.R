skip_on_cran()
skip_if_not_installed("visreg")
# False alarm in R-devel

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

p0 <- plotmod(output = lm_out,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        w_values = c(9, -3, 2))
p0

p0_check <- structure(list(x = c(1.95023867728441, 1.95023867728441, 1.95023867728441
), xend = c(3.47616132271559, 3.47616132271559, 3.47616132271559
), y = c(9.84511127666008, 5.94229410301828, 3.154567550417),
    yend = c(7.31044167268714, 6.94744412766431, 6.68816016693373
    )), class = "data.frame", row.names = c(NA, -3L))

test_that("Check plotmod lm", {
  expect_equal(layer_data(p0, 2)[, colnames(p0_check)],
               p0_check)
  })

p1 <- plotmod(output = lm_std,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        w_values = c(2.5, -1.5, 2.0))
p1

p1_check <- structure(list(x = c(-1, -1, -1), xend = c(1, 1, 1), y = c(0.536888611934098,
0.417491886093573, -0.4182851947901), yend = c(0.225457080818834,
0.214352097819225, 0.136617216821964)), class = "data.frame", row.names = c(NA,
-3L))

test_that("Check plotmod lm, std", {
  expect_equal(layer_data(p1, 2)[, colnames(p1_check)],
               p1_check)
  })

