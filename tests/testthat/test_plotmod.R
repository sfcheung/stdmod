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
        y_label = "Sleep Duration")
p0
p0 + coord_fixed(ratio = (1/.3)*sd(dat$emotional_stability)/sd(dat$sleep_duration))

p0_check <- structure(list(x = c(1.95023867728441, 1.95023867728441), xend = c(3.47616132271559, 
3.47616132271559), y = c(6.35285945290502, 7.02951847531317),
    yend = c(6.98563044473979, 7.04856589701016)), class = "data.frame", row.names = c(NA,
-2L))

test_that("Check plotmod lm", {
  expect_equal(layer_data(p0, 2)[, 2:5],
               p0_check)
  })

p0ylim <- layer_scales(p0)$y$range$range
p0xlim <- layer_scales(p0)$x$range$range
wlo <- mean(dat$conscientiousness) - sd(dat$conscientiousness)
whi <- mean(dat$conscientiousness) + sd(dat$conscientiousness)

visreg::visreg(lm_out, "emotional_stability", "conscientiousness",
            breaks = c(wlo, whi), overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
            xlim(p0xlim[1], p0xlim[2]) + ylim(p0ylim[1], p0ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p0

p0p <- plotmod(output = lm_out,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        w_method = "percentile",
        w_percentiles = c(.10, .90))

p0p_check <- structure(list(x = c(1.95023867728441, 1.95023867728441), xend = c(3.47616132271559, 
3.47616132271559), y = c(6.22106675827842, 7.11313925511082),
    yend = c(6.97337252373737, 7.05634339117116)), class = "data.frame", row.names = c(NA,
-2L))

test_that("Check plotmod lm, percentiles", {
  expect_equal(layer_data(p0p, 2)[, 2:5],
               p0p_check)
  })

p0pylim <- layer_scales(p0p)$y$range$range
p0pxlim <- layer_scales(p0p)$x$range$range
visreg::visreg(lm_out, "emotional_stability", "conscientiousness",
            overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
            xlim(p0pxlim[1], p0pxlim[2]) + ylim(p0pylim[1], p0pylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p0p

# library(sjPlot)
# p <- plot_model(lm_out,
#            type = "pred", terms = c("emotional_stability",
#                                     "conscientiousness"),
#            mdrt.values = "meansd",
#            ci.lvl = NA)
# p + xlim(p0xlim[1], p0xlim[2]) + ylim(p0ylim[1], p0ylim[2]) +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))

# library(interactions)
# interact_plot(model = lm_out,
#               pred = emotional_stability,
#               modx = conscientiousness) +
#               xlim(p0xlim[1], p0xlim[2]) + ylim(p0ylim[1], p0ylim[2]) +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))

p1 <- plotmod(output = lm_std,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")
p1
p1 + coord_fixed(ratio = (1/.3))

p1_check <- structure(list(x = c(-1, -1), xend = c(1, 1), y = c(-0.298888468949575, 
0.178698434412524), yend = c(0.147722199821573, 0.192142131820008
)), class = "data.frame", row.names = c(NA, -2L))

test_that("Check plotmod lm, std", {
  expect_equal(layer_data(p1, 2)[, 2:5],
               p1_check)
  })


p1ylim <- layer_scales(p1)$y$range$range
p1xlim <- layer_scales(p1)$x$range$range

visreg::visreg(lm_std_check, "emotional_stability", "conscientiousness",
            breaks = c(-1, 1), overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
            xlim(p1xlim[1], p1xlim[2]) + ylim(p1ylim[1], p1ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p1


p1p <- plotmod(output = lm_std,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration",
        w_method = "percentile",
        w_percentiles = c(.10, .90))
p1p

p1p_check <- structure(list(x = c(-1, -1), xend = c(1, 1), y = c(-0.391907940837296, 
0.237718100090162), yend = c(0.139070541690466, 0.197631498294497
)), class = "data.frame", row.names = c(NA, -2L))

test_that("Check plotmod lm, std, percentiles", {
  expect_equal(layer_data(p1p, 2)[, 2:5],
               p1p_check)
  })



p1pylim <- layer_scales(p1p)$y$range$range
p1pxlim <- layer_scales(p1p)$x$range$range

visreg::visreg(lm_std_check, "emotional_stability", "conscientiousness",
            overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
            xlim(p1pxlim[1], p1pxlim[2]) + ylim(p1pylim[1], p1pylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p1p


# p <- plot_model(lm_std,
#            type = "pred", terms = c("emotional_stability",
#                                     "conscientiousness"),
#            mdrt.values = "meansd",
#            ci.lvl = NA)
# p + xlim(p1xlim[1], p1xlim[2]) + ylim(p1ylim[1], p1ylim[2]) +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))

# interact_plot(model = lm_std,
#               pred = emotional_stability,
#               modx = conscientiousness) +
#               xlim(p1xlim[1], p1xlim[2]) + ylim(p1ylim[1], p1ylim[2]) +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))
# p1

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

p0_wcat <- plotmod(output = lm_out_wcat,
        x = emotional_stability,
        w = gender,
        x_label = "Emotional Stability",
        w_label = "Gender",
        y_label = "Sleep Duration")
p0_wcat

p0_wcat_check <- structure(list(x = c(1.95023867728441, 1.95023867728441), xend = c(3.47616132271559, 
3.47616132271559), y = c(6.62226556199772, 6.59876556910144),
    yend = c(7.02520488312402, 6.54759945860682)), class = "data.frame", row.names = c(NA,
-2L))

test_that("Check plotmod lm, std, percentiles", {
  expect_equal(layer_data(p0_wcat, 2)[, 2:5],
               p0_wcat_check)
  })



p0wcatylim <- layer_scales(p0_wcat)$y$range$range
p0wcatxlim <- layer_scales(p0_wcat)$x$range$range

p1_wcat <- plotmod(output = lm_std_wcat,
        x = emotional_stability,
        w = gender,
        x_label = "Emotional Stability",
        w_label = "Gender",
        y_label = "Sleep Duration")
p1_wcat

p1wcatylim <- layer_scales(p1_wcat)$y$range$range
p1wcatxlim <- layer_scales(p1_wcat)$x$range$range

visreg::visreg(lm_out_wcat, "emotional_stability", "gender",
            overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
            xlim(p0wcatxlim[1], p0wcatxlim[2]) + ylim(p0wcatylim[1], p0wcatylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p0_wcat

# library(sjPlot)
# p <- plot_model(lm_out_wcat,
#            type = "pred", terms = c("emotional_stability",
#                                     "gender"),
#            mdrt.values = "meansd",
#            ci.lvl = NA)
# p +  xlim(p0wcatxlim[1], p0wcatxlim[2]) + ylim(p0wcatylim[1], p0wcatylim[2])  +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))
# p0_wcat

# interact_plot(model = lm_out_wcat,
#               pred = emotional_stability,
#               modx = gender) +
#               xlim(p0wcatxlim[1], p0wcatxlim[2]) + ylim(p0wcatylim[1], p0wcatylim[2]) +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))
# p0_wcat

visreg::visreg(lm_std_wcat_check, "emotional_stability", "gender",
            overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
           xlim(p1wcatxlim[1], p1wcatxlim[2]) + ylim(p1wcatylim[1], p1wcatylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p1_wcat

# p <- plot_model(lm_std_wcat,
#            type = "pred", terms = c("emotional_stability",
#                                     "gender"),
#            ci.lvl = NA)
# p + xlim(p1wcatxlim[1], p1wcatxlim[2]) + ylim(p1wcatylim[1], p1wcatylim[2]) +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))
# p1_wcat

# interact_plot(model = lm_std_wcat,
#               pred = emotional_stability,
#               modx = gender) +
#                xlim(p1wcatxlim[1], p1wcatxlim[2]) + ylim(p1wcatylim[1], p1wcatylim[2])  +
#                theme(legend.position = "top",
#                      plot.caption = element_text(hjust = .5),
#                      plot.title = element_text(hjust = .5),
#                      plot.subtitle = element_text(hjust = .5))
# p1_wcat


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

p0_wcat3 <- plotmod(output = lm_out_wcat3,
        x = emotional_stability,
        w = city,
        x_label = "Emotional Stability",
        w_label = "City",
        y_label = "Sleep Duration")
p0_wcat3


p0_wcat_check <- structure(list(x = c(1.95023867728441, 1.95023867728441, 1.95023867728441
), xend = c(3.47616132271559, 3.47616132271559, 3.47616132271559
), y = c(6.94522100731477, 6.51682495584239, 6.39424169437123
), yend = c(7.08950964764363, 6.83129421896357, 6.88894193948001
)), class = "data.frame", row.names = c(NA, -3L))

test_that("Check plotmod lm, std, percentiles", {
  expect_equal(layer_data(p0_wcat3, 2)[, 2:5],
               p0_wcat_check)
  })


p0wcat3ylim <- layer_scales(p0_wcat3)$y$range$range
p0wcat3xlim <- layer_scales(p0_wcat3)$x$range$range

p1_wcat3 <- plotmod(output = lm_std_wcat3,
        x = emotional_stability,
        w = city,
        x_label = "Emotional Stability",
        w_label = "City",
        y_label = "Sleep Duration")
p1_wcat3

p1_wcat3_check <- structure(list(x = c(-1, -1, -1), xend = c(1, 1, 1), y = c(0.119201161237405, 
-0.183161383057221, -0.269680823885312), yend = c(0.221040289428956,
0.0387914714870233, 0.0794793148698632)), class = "data.frame", row.names = c(NA, 
-3L))

test_that("Check plotmod lm, std, percentiles", {
  expect_equal(layer_data(p1_wcat3, 2)[, 2:5],
               p1_wcat3_check)
  })


p1wcat3ylim <- layer_scales(p1_wcat3)$y$range$range
p1wcat3xlim <- layer_scales(p1_wcat3)$x$range$range

visreg::visreg(lm_out_wcat3, "emotional_stability", "city",
            overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
            xlim(p0wcat3xlim[1], p0wcat3xlim[2]) + ylim(p0wcat3ylim[1], p0wcat3ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p0_wcat3

visreg::visreg(lm_std_wcat3_check, "emotional_stability", "city",
            overlay = TRUE,
            partial = FALSE,
            band = FALSE,
            rug = FALSE,
            gg = TRUE) +
           xlim(p1wcat3xlim[1], p1wcat3xlim[2]) + ylim(p1wcat3ylim[1], p1wcat3ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p1_wcat3
