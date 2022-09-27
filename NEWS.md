# stdmod 0.2.0.1

- Fixed a bug in `std_selected()`: It now works correctly when
  a variable in the data frame is a factor.

# stdmod 0.2.0.0

- Changed the default of `confint()` and
  `vcov()` for `std_selected`-class object.
  If bootstrap CIs are requested, then bootstrap CIs
  and VCOV based on bootstrapping should be returned. (0.2.0.0)


# stdmod 0.1.7.7

- Updated references. (0.1.7.5)
- Minor changes due to Roxygen updates. (0.1.7.5)
- Added R CMD Check action (0.1.7.6)
- Minor changes due to Roxygen updates. (0.1.7.7)
- Fixed a typo in CITATION

# stdmod 0.1.7.4

(All major changes after 0.1.7.1)

- Fixed a bug in `plotmod()`. It now correctly handles more than two levels
  when `w_method` is set to`"percentile"`. (0.1.7.2, 0.1.7.3)
- Fixed a typo in DESCRIPTION. (0.1.7.4)

# stdmod 0.1.7.1

(All major changes after 0.1.5)

- Added `plotmod()` for plotting moderation effects. This function will check
  whether a variable is standardized. If yes, will note this in the plot.
- `plotmod()` can also plot a Tumble graph (Bodner, 2016) if `graph_type` is
  set to `"tumble"`.
- Updated vignettes to use `plotmod()` instead of `visreg::visreg()`.
- Added `cond_effect()` for computing conditional effects. This function
  will check which variable(s) is/are standardized. If yes, will note
  this in the printout.
- Added `cond_effect_boot()`, a wrapper of
  `cond_effect()` that can form nonparametric bootstrap confidence intervals
  for the conditional effects, which may be partially or completely
  standardized.
- Updated the print method for the summary of `std_selected()` and
  `std_selected_boot()`.
- Added more vignettes to introduce the new functions.
- `stdmod_lavaan()` now returns an object of the class `stdmod_lavaan`,
  with methods print, confint, and coef added.

# stdmod 0.1.5

- Changed the column order of the coefficient table
  in `std_selected_boot()` output. Bootstrap confidence
  intervals are placed next to parameter estimates.
- Added `vcov()` method for `std_selected()` output. If bootstrapping is used,
  it can return the variance-covariance matrix of the bootstrap estimates.
- Added `confint()` method for `std_selected()` output. If bootstrapping is used,
  it can return the bootstrap percentile confidence intervals if requested.
- Other minor fixes.

# stdmod 0.1.3

- Set up gh-pages and fix dependency in vignette.

# stdmod 0.1.2

- Updated the methods for `std_selected()`.

# stdmod 0.1.1

- The first version.
