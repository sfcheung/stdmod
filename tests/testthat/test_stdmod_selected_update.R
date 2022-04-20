skip("WIP")

# This test will not be used. Currently, update() will return an error because
# it is better to do update before calling std_selected() or std_selected_boot().

library(testthat)
library(stdmod)

context("Check update method")

dat <- test_x_1_w_1_v_1_cat1_n_500

lm_0 <- lm(dv ~ iv*mod , dat)

set.seed(649831074)
stdmod_0 <- std_selected_boot(lm_0, to_scale = ~ mod + dv, to_center = ~ mod + dv,
                               nboot = 100,
                               full_output = TRUE)
summary(stdmod_0)
termlabels_0std <- eval(parse(text = deparse(attr(terms(stdmod_0), "term.labels"))))

lm_1 <- update(lm_0, . ~  . + v1)
summary(lm_1)
termlabels_1 <- eval(parse(text = deparse(attr(terms(lm_1), "term.labels"))))
termvariables_1 <- deparse(attr(terms(lm_1), "variables"))
resp_1 <- attr(terms(lm_1), "response")

lm_2 <- update(lm_0, . ~  . - mod)
summary(lm_2)
termlabels_2 <- eval(parse(text = deparse(attr(terms(lm_2), "term.labels"))))
termvariables_2 <- deparse(attr(terms(lm_2), "variables"))
resp_2 <- attr(terms(lm_2), "response")

lm_3 <- update(lm_0, v1 ~ .)
summary(lm_3)
termlabels_3 <- eval(parse(text = deparse(attr(terms(lm_3), "term.labels"))))
termvariables_3 <- deparse(attr(terms(lm_3), "variables"))
resp_3 <- attr(terms(lm_3), "response")

stdmod_1 <- update(stdmod_0, . ~ . + v1)
termlabels_1std <- eval(parse(text = deparse(attr(terms(stdmod_1), "term.labels"))))
termvariables_1std <- deparse(attr(terms(stdmod_1), "variables"))
resp_1std <- attr(terms(stdmod_1), "response")

stdmod_2 <- update(stdmod_0, . ~ . - mod)
termlabels_2std <- eval(parse(text = deparse(attr(terms(stdmod_2), "term.labels"))))
termvariables_2std <- deparse(attr(terms(stdmod_2), "variables"))
resp_2std <- attr(terms(stdmod_2), "response")

stdmod_3 <- update(stdmod_0, v1 ~ .)
termlabels_3std <- eval(parse(text = deparse(attr(terms(stdmod_3), "term.labels"))))
termvariables_3std <- deparse(attr(terms(stdmod_3), "variables"))
resp_3std <- attr(terms(stdmod_3), "response")

test_that("Check terms labels 1", {
    expect_equivalent(
        termlabels_1std, termlabels_1
      )
  })

test_that("Check terms labels 2", {
    expect_equivalent(
        termlabels_2std, termlabels_2
      )
  })

test_that("Check terms labels 3", {
    expect_equivalent(
        termlabels_3std, termlabels_3
      )
  })

test_that("Check terms variables 1", {
    expect_equivalent(
        termvariables_1std, termvariables_1
      )
  })

test_that("Check terms variables 2", {
    expect_equivalent(
        termvariables_2std, termvariables_2
      )
  })

test_that("Check terms variables 3", {
    expect_equivalent(
        termvariables_3std, termvariables_3
      )
  })
