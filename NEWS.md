# stdmod 0.1.5

- 0.1.5

    - Change the column order of the coefficient table
      in `std_selected_boot` output. Bootstrap confidence
      intervals are placed next to parameter estimates.
    - Add `vcov` method for `std_selected` output. If bootstrapping is used,
      it can return the variance-covariance matrix of the bootstrap estimates.
    - Add `confint` method for `std_selected` output. If bootstrapping is used,
      it can return the bootstrap percentile confidence intervals if requested.
    - Other minor fixes.

- 0.1.3

    - Setup gh-pages and fix dependency in vignette.

- 0.1.2

    - Update the methods for `std_selected`.

# stdmod 0.1.1

- The first version.
