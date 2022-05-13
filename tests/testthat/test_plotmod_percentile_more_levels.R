skip("WIP")
library(testthat)
library(stdmod)
library(ggplot2)

# Manual check

lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        graph_type = "tumble",
        w_method = "percentile",
        w_percentiles = c(.16, .50, .84))
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        graph_type = "tumble",
        w_method = "percentile",
        w_percentiles = c(.16, .30, .50, .70, .84))
lm_raw <- lm(sleep_duration ~ age + gender +
                              emotional_stability * conscientiousness,
             sleep_emo_con)
plotmod(lm_raw,
        x = "emotional_stability",
        w = "conscientiousness",
        w_method = "percentile",
        w_percentiles = c(.16, .30, .50, .40, .84))
