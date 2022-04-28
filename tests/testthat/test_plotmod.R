skip("WIP")

library(stdmod)
library(testthat)

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
summary(lm_xw_centered)


plotmod(output = lm_xw_centered,
        x = emotional_stability,
        w = conscientiousness,
        x_label = "Emotional Stability",
        w_label = "Conscientiousness",
        y_label = "Sleep Duration")
