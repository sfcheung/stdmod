# stdmod 0.1.6 (latest: 0.1.6.1003)

- Add `plotmod()` for plotting moderation effects. This function will check
  whether a variable is standardized. If yes, will note this in the plot.
- Update vignettes to use `plotmod()` instead of `visreg::visreg()`.
- Add `cond_effect()` for computing conditional effects. This function
  will check which variable(s) is/are standardized. If yes, will note
  this in the printout.
- Update the print method for the summary of `std_selected()` and
  `std_selected_boot()`.


# stdmod 0.1.5

- Change the column order of the coefficient table
  in `std_selected_boot()` output. Bootstrap confidence
  intervals are placed next to parameter estimates.
- Add `vcov()` method for `std_selected()` output. If bootstrapping is used,
  it can return the variance-covariance matrix of the bootstrap estimates.
- Add `confint()` method for `std_selected()` output. If bootstrapping is used,
  it can return the bootstrap percentile confidence intervals if requested.
- Other minor fixes.

# stdmod 0.1.3

- Setup gh-pages and fix dependency in vignette.

# stdmod 0.1.2

- Update the methods for `std_selected()`.

# stdmod 0.1.1

- The first version.
