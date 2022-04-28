skip("WIP")

library(stdmod)
library(testthat)
library(ggplot2)

dat <- test_x_1_w_1_v_1_cat1_n_500
head(dat)

# Do a moderated regression by lm
lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
summary(lm_raw)
# Standardize all variables as in std_selected above, and compute the
# nonparametric bootstrapping percentile confidence intervals.
lm_std_boot <- std_selected_boot(lm_raw,
                                 to_scale = ~ .,
                                 to_center = ~ .,
                                 conf = .95,
                                 nboot = 100)
# In real analysis, nboot should be at least 2000.
summary(lm_std_boot)


lm_out <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness, sleep_emo_con)
lm_xw_centered <- std_selected(lm_out, to_center = ~ emotional_stability + conscientiousness)
lm_out_check <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness,
                   model.frame(lm_xw_centered))

p0 <- plotmod(output = lm_xw_centered,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")
p0
p0ylim <- layer_scales(p0)$y$range$range
p0xlim <- layer_scales(p0)$x$range$range

summary(lm_xw_centered)
visreg::visreg(lm_xw_centered, "emotional_stability", "conscientiousness",
            breaks = 2, overlay = TRUE)


tmp <- model.frame(lm_xw_centered)
lm_out_check <- lm(sleep_duration ~ age + gender + emotional_stability*conscientiousness,
                   tmp)
all.equal(coef(lm_out_check), coef(lm_xw_centered))
visreg::visreg(lm_out_check, "emotional_stability", "conscientiousness",
            breaks = 2, overlay = TRUE)
wlo <- mean(tmp$conscientiousness) - sd(tmp$conscientiousness)
whi <- mean(tmp$conscientiousness) + sd(tmp$conscientiousness)
xlo <- mean(tmp$emotional_stability) - sd(tmp$emotional_stability)
xhi <- mean(tmp$emotional_stability) + sd(tmp$emotional_stability)
visreg::visreg(lm_out_check, "emotional_stability", "conscientiousness",
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


library(sjPlot)
p <- plot_model(lm_out_check,
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
interact_plot(model = lm_out_check,
              pred = emotional_stability,
              modx = conscientiousness) +
              xlim(p0xlim[1], p0xlim[2]) + ylim(p0ylim[1], p0ylim[2]) +
               theme(legend.position = "top",
                     plot.caption = element_text(hjust = .5),
                     plot.title = element_text(hjust = .5),
                     plot.subtitle = element_text(hjust = .5))

lm_std <- std_selected(lm_out, to_center = ~ .,
                               to_scale = ~ .)
summary(lm_std)

plotmod(output = lm_std,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")
