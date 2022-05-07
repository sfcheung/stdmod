#' @title Sample Dataset: A Path Model With A Moderator
#'
#' @description For testing the handling of warning
#' in [stdmod_lavaan()]. Generated from the following
#' model. `dv` has about 88% missing.
#'
#'```
#'mod <-
#'"
#'med ~ iv + mod + iv:mod + cov1
#'dv ~ med + cov2
#'"
#'```
#'
#' @format A data frame with 500 rows and 6 variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{med}{Mediator, continuous}
#'   \item{mod}{Moderator, continuous}
#'   \item{cov1}{Covariate, continuous}
#'   \item{cov2}{Covariate, continuous}
#' }
#'
"test_mod3_miss"
