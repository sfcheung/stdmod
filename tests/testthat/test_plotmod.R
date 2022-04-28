skip("WIP")

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

library(sjPlot)
p <- plot_model(lm_out,
           type = "pred", terms = c("emotional_stability",
                                    "conscientiousness"),
           mdrt.values = "meansd",
           ci.lvl = NA)
p + xlim(p0xlim[1], p0xlim[2]) + ylim(p0ylim[1], p0ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))

library(interactions)
interact_plot(model = lm_out,
              pred = emotional_stability,
              modx = conscientiousness) +
              xlim(p0xlim[1], p0xlim[2]) + ylim(p0ylim[1], p0ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))

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
p <- plot_model(lm_std,
           type = "pred", terms = c("emotional_stability",
                                    "conscientiousness"),
           mdrt.values = "meansd",
           ci.lvl = NA)
p + xlim(p1xlim[1], p1xlim[2]) + ylim(p1ylim[1], p1ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))

interact_plot(model = lm_std,
              pred = emotional_stability,
              modx = conscientiousness) +
              xlim(p1xlim[1], p1xlim[2]) + ylim(p1ylim[1], p1ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))
p1
