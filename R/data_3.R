#' @title Sample Dataset: A Path Model With A Moderator
#'
#' @description For testing. Generated from the following
#' model.
#'
#'```
#'mod <-
#'"
#'med ~ iv + mod + iv:mod + cov1
#'dv ~ med + cov2
#'"
#'```
#'
#' @format A data frame with 300 rows and 6 variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{med}{Mediator, continuous}
#'   \item{mod}{Moderator, continuous}
#'   \item{cov1}{Covariate, continuous}
#'   \item{cov2}{Covariate, continuous}
#' }
#'
"test_mod1"

#' @title Sample Dataset: A Path Model With A Moderator
#'
#' @description For testing. Generated from the following
#' model.
#'
#'```
#'mod <-
#'"
#'med ~ iv + cov1
#'dv ~ med + mod + med:mod + cov2
#'"
#'```
#'
#' @format A data frame with 300 rows and 6 variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{med}{Mediator, continuous}
#'   \item{mod}{Moderator, continuous}
#'   \item{cov1}{Covariate, continuous}
#'   \item{cov2}{Covariate, continuous}
#' }
#'
"test_mod2"
