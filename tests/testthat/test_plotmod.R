skip("WIP")
skip_if(!interactive(),
        message = "plotmod not tested if not interactive")


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
p0p
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
p1ylim <- layer_scales(p1)$y$range$range
p1xlim <- layer_scales(p1)$x$range$range

visreg::visreg(lm_std, "emotional_stability", "conscientiousness",
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
p1pylim <- layer_scales(p1p)$y$range$range
p1pxlim <- layer_scales(p1p)$x$range$range

visreg::visreg(lm_std, "emotional_stability", "conscientiousness",
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

visreg::visreg(lm_std_wcat, "emotional_stability", "gender",
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

p0wcat3ylim <- layer_scales(p0_wcat3)$y$range$range
p0wcat3xlim <- layer_scales(p0_wcat3)$x$range$range

p1_wcat3 <- plotmod(output = lm_std_wcat3,
        x = emotional_stability,
        w = city,
        x_label = "Emotional Stability",
        w_label = "City",
        y_label = "Sleep Duration")
p1_wcat3

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

visreg::visreg(lm_std_wcat3, "emotional_stability", "city",
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
