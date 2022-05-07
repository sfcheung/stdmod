#' @title Sample Dataset: One IV, One Moderator, Two Covariates
#'
#' @description The independent variable and the moderator are
#'  associated. For demonstrating the use of tumble graph.
#'
#' @format A data frame with 500 rows and 5 variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{mod}{Moderator variable, continuous}
#'   \item{v1}{Covariate, continuous}
#'   \item{cat1}{Covariate, categorical (string) with three values, "gp1",
#'                 "gp2", and "gp3"}
#' }
#'
"test_x_1_w_1_v_1_cat1_xw_cov_n_500"

#' @title Sample Dataset: One IV, One 3-Category Moderator, Two Covariates
#'
#' @description The independent variable and the categorical
#'  moderator are
#'  associated. For demonstrating the use of tumble graph.
#'
#' @format A data frame with 500 rows and 5 variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{mod}{Moderator variable, categorical (string) with three categories,
#'              "City Alpha", "City Gamma", and "City Beta"}
#'   \item{v1}{Covariate, continuous}
#'   \item{cat1}{Covariate, categorical (string) with three values, "gp1",
#'               "gp2", and "gp3"}
#' }
#'
"test_x_1_w_1_v_1_cat1_xw_cov_wcat3_n_500"