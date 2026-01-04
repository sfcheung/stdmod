# Package index

## Standardized Moderation Effect

Standardize selected variables and form nonparametric bootstrapping
confidence intervals.
[`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
and
[`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
also support mean-centering and regression models without interaction
terms.
[`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md)
provides preliminary support for interaction terms in a model fitted by
[`lavaan::lavaan()`](https://rdrr.io/pkg/lavaan/man/lavaan.html).

- [`std_selected()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  [`std_selected_boot()`](https://sfcheung.github.io/stdmod/reference/std_selected.md)
  : Standardize Variables in a Regression Model
- [`stdmod_lavaan()`](https://sfcheung.github.io/stdmod/reference/stdmod_lavaan.md)
  : Standardized Moderation Effect and Its Bootstrap CI in 'lavaan'
- [`stdmod()`](https://sfcheung.github.io/stdmod/reference/stdmod.md)
  [`stdmod_boot()`](https://sfcheung.github.io/stdmod/reference/stdmod.md)
  : Standardized Moderation Effect Given an 'lm' Output

## Helper Functions

Plot moderation effects or compute conditional effects for specific
levels of the moderators.

- [`plotmod()`](https://sfcheung.github.io/stdmod/reference/plotmod.md)
  : Moderation Effect Plot
- [`cond_effect()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  [`cond_effect_boot()`](https://sfcheung.github.io/stdmod/reference/cond_effect.md)
  : Conditional Effects

## Other Functions

- [`summary(`*`<std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/summary.std_selected.md)
  : Summary Method for a 'std_selected' Class Object
- [`update(`*`<std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/update.std_selected.md)
  : The 'update' Method for a 'std_selected' Class Object
- [`vcov(`*`<std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/vcov.std_selected.md)
  : The 'vcov' Method for a 'std_selected' Class Object
- [`add1(`*`<std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/add1.std_selected.md)
  : The 'add1' Method for a 'std_selected' Class Object
- [`confint(`*`<std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/confint.std_selected.md)
  : Confidence Intervals for a 'std_selected' Class Object
- [`print(`*`<std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/print.std_selected.md)
  : Print Basic Information of a 'std_selected' Class Object
- [`print(`*`<cond_effect>`*`)`](https://sfcheung.github.io/stdmod/reference/print.cond_effect.md)
  : Print a 'cond_effect' Class Object
- [`coef(`*`<cond_effect>`*`)`](https://sfcheung.github.io/stdmod/reference/coef.cond_effect.md)
  : Conditional Effect in a 'cond_effect'-Class Object
- [`confint(`*`<cond_effect>`*`)`](https://sfcheung.github.io/stdmod/reference/confint.cond_effect.md)
  : Confidence Intervals for a 'cond_effect' Class Object
- [`print(`*`<summary.std_selected>`*`)`](https://sfcheung.github.io/stdmod/reference/print.summary.std_selected.md)
  : Print the Summary of a 'std_selected' Class Object
- [`confint(`*`<stdmod_lavaan>`*`)`](https://sfcheung.github.io/stdmod/reference/confint.stdmod_lavaan.md)
  : Confidence Intervals for a 'stdmod_lavaan' Class Object
- [`print(`*`<stdmod_lavaan>`*`)`](https://sfcheung.github.io/stdmod/reference/print.stdmod_lavaan.md)
  : Print a 'stdmod_lavaan' Class Object
- [`coef(`*`<stdmod_lavaan>`*`)`](https://sfcheung.github.io/stdmod/reference/coef.stdmod_lavaan.md)
  : Standardized Moderation Effect in a 'stdmod_lavaan' Class Object

## Datasets

Datasets used in examples.

- [`sleep_emo_con`](https://sfcheung.github.io/stdmod/reference/sleep_emo_con.md)
  : Sample Dataset: Predicting Sleep Duration
- [`test_mod1`](https://sfcheung.github.io/stdmod/reference/test_mod1.md)
  : Sample Dataset: A Path Model With A Moderator
- [`test_mod2`](https://sfcheung.github.io/stdmod/reference/test_mod2.md)
  : Sample Dataset: A Path Model With A Moderator
- [`test_mod3_miss`](https://sfcheung.github.io/stdmod/reference/test_mod3_miss.md)
  : Sample Dataset: A Path Model With A Moderator
- [`test_x_1_w_1_v_1_cat1_n_500`](https://sfcheung.github.io/stdmod/reference/test_x_1_w_1_v_1_cat1_n_500.md)
  : Sample Dataset: One IV, One Moderator, Two Covariates
- [`test_x_1_w_1_v_1_cat1_xw_cov_n_500`](https://sfcheung.github.io/stdmod/reference/test_x_1_w_1_v_1_cat1_xw_cov_n_500.md)
  : Sample Dataset: One IV, One Moderator, Two Covariates
- [`test_x_1_w_1_v_1_cat1_xw_cov_wcat3_n_500`](https://sfcheung.github.io/stdmod/reference/test_x_1_w_1_v_1_cat1_xw_cov_wcat3_n_500.md)
  : Sample Dataset: One IV, One 3-Category Moderator, Two Covariates
- [`test_x_1_w_1_v_2_n_500`](https://sfcheung.github.io/stdmod/reference/test_x_1_w_1_v_2_n_500.md)
  : Sample Dataset: One IV, One Moderator, Two Covariates
