# stdmod

This package includes functions for computing a standardized moderation effect and forming its confidence interval by nonparametric bootstrapping correctly. It was described briefly in the following publication:

Cheung, S. F., Cheung, S.-H., Lau, E. Y. Y., Hui, C. H., & Vong, W. N. (2022) Improving an old way to measure moderation effect in standardized units. Advance online publication. *Health Psychology*. [DOI to be included].

# Installation

Stable release versions of this package can be downloaded below:

https://github.com/sfcheung/stdmod/releases

The latest developmental version of this package can be installed by `remotes::install_github`:

```
remotes::install_github("sfcheung/stdmod")
```

# Implementation

The main function, `stdmod_selected()`, accepts an `lm()`
output, standardizes variables by users, and update the
results. If interaction terms are presented, they will be
formed after the standardization. If bootstrapping
confidence intervals are requested, both standardization
and regression will be repeated in each bootstrap sample,
ensuring that the sampling variability of the standardizers
(i.e., the standard deviations of the selected variables),
are also taken into account.