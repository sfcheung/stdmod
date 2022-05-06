# stdmod 0.1.6

 (latest: 0.1.6.1010)

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
  with a print method.

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
