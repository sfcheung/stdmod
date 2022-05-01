#' @title Sample Dataset: One IV, One Moderator, Two Covariates
#'
#' @description All variables are continuous. For testing.
#'
#' @format A data frame with 500 rows and five variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{mod}{Moderator variable, continuous}
#'   \item{v1}{Covariate, continuous}
#'   \item{v2}{Covariate, continuous}
#' }
"test_x_1_w_1_v_2_n_500"

#' @title Sample Dataset: One IV, One Moderator, Two Covariates
#'
#' @description The IV (iv1) continuous; The moderator (cat2) is
#' categorical. For testing.
#'
#' @format A data frame with 500 rows and five variables:
#' \describe{
#'   \item{dv}{Dependent variable, continuous}
#'   \item{iv}{Independent variable, continuous}
#'   \item{mod}{Moderator variable, continuous}
#'   \item{v1}{Covariate, continuous}
#'   \item{cat1}{Covariate, categorical}
#' }
#'
"test_x_1_w_1_v_1_cat1_n_500"

#' @title Sample Dataset: Predicting Sleep Duration
#'
#' @description A random subset from a real dataset. For
#' illustration.
#'
#' @format A data frame with 500 rows and six variables:
#' \describe{
#'   \item{case_id}{Case ID, integer}
#'   \item{sleep_duration}{Sleep duration in hours}
#'   \item{conscientiousness}{Conscientiousness score, continuous}
#'   \item{emotional_stability}{Emotional stability score, continuous}
#'   \item{age}{Age in years}
#'   \item{gender}{Gender, string, "female" or "male"}
#' }
#'
"sleep_emo_con"