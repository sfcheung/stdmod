#' @title The 'update' Method for a 'std_selected' Class Object
#'
#' @description This should be used only to update the call to [lm()],
#'          not to the call to [std_selected()] or [std_selected_boot()].
#'
#' @details Although supported, it is not recommended to update an analysis
#'          processed by [std_selected()] or [std_selected_boot()]. It is
#'          recommended to call [lm()] again and pass the output to
#'          [std_selected()] or [std_selected_boot()].
#'
#' @param object The output of the class [std_selected()].
#' @param formula. Changes to the formula.
#' @param ...  Optional arguments to be changed.
#' @param evaluate Whether the call will be evaluated.
#'
#' @return If `evaluate` = `TRUE`, it returns the updated fitted object,
#'         otherwise, the updated call.
#'
#' @author Shu Fai Cheung <https://orcid.org/0000-0002-9871-9448>
#'
#' @examples
#'
#' # Load a sample data set
#'
#' dat <- test_x_1_w_1_v_1_cat1_n_500
#' head(dat)
#'
#' # Do a moderated regression by lm
#' lm_raw <- lm(dv ~ iv*mod + v1 + cat1, dat)
#' summary(lm_raw)
#'
#' # Standardize all variables except for categorical variables.
#' # Interaction terms are formed after standardization.
#' lm_std <- std_selected(lm_raw, to_scale = ~ .,
#'                                to_center = ~ .)
#' summary(lm_std)
#'
#' # Update the model
#' lm_std2 <- update(lm_std, . ~ . - v1)
#' summary(lm_std2)
#'
#' @export

update.std_selected <- function(object, formula., ..., evaluate = TRUE) {
    # Adapted from update.default in stats
    # if (is.null(call <- stats::getCall(object))) {
    #     stop("need an object with call component")
    #   }
    call <- object$lm_out_call
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.)) {
        call$formula <- stats::update.formula(stats::formula(object), formula.)
      }
    if(length(extras)) {
        existing <- !is.na(match(names(extras), names(call)))
        ## do these individually to allow NULL to remove entries.
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if(any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
          }
      }
    lm_out_call <- call
    if (is.null(extras$data)) {
        lm_out_call$data <- object$lm_out_call$data
      }
    if (!is.null(object$std_selected_boot_call)) {
        new_call <- object$std_selected_boot_call
        new_call$lm_out <- lm_out_call
      } else {
        new_call <- object$std_selected_call
        new_call$lm_out <- lm_out_call
      }
    if (evaluate) {
        # lm_out <- eval(lm_out_call, parent.frame())
        out <- eval(new_call, parent.frame())
        return(out)
      } else {
        return(new_call)
      }
  }
