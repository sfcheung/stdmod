# stdmod: Standardized Moderation

This package includes functions for computing a standardized
moderation effect and forming its confidence interval by
nonparametric bootstrapping correctly. It was described briefly
in the following publication ([OSF project page](https://osf.io/ac8de/)).
It supports moderated regression conducted by `stats::lm()` and path
analysis with product term conducted by `lavaan::lavaan()`.

- Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N. (2022) Improving an old way to measure moderation effect in standardized units. Advance online publication. *Health Psychology*. https://doi.org/10.1037/hea0001188.

More information on this package:

https://sfcheung.github.io/stdmod/

## Quick Links:

- `vignette("stdmod")`: A quick start on how to use
  `std_selected()` and `std_selected_boot()`, the
  two main functions.

- `vignette("numerical_example_01")`: Demonstrates
  how `std_selected()` and `std_selected_boot()` can
  also be used to mean center or standardize selected
  variables in any regression models, not just moderated
  regression models, and how `std_selected_boot()` can be
  used to form nonparametric bootstrap confidence intervals
  for standardized regression coefficients (*betas* in
  psychology literature).

# Installation

Stable release versions of this package can be downloaded below:

https://github.com/sfcheung/stdmod/releases

The latest developmental version of this package at GitHub can be
installed by `remotes::install_github()`:

```
remotes::install_github("sfcheung/stdmod")
```

# Implementation

The main function, `std_selected()`, accepts an `lm()`
output, standardizes variables by users, and update the
results. If interaction terms are present, they will be
formed after the standardization. If bootstrap
confidence intervals are requested using
`std_selected_boot()`, both standardization
and regression will be repeated in each bootstrap sample,
ensuring that the sampling variability of the standardizers
(e.g., the standard deviations of the selected variables),
are also taken into account.

# Issues

If you have any suggestions and found any bugs, please feel
feel to open a GitHub issue. Thanks.