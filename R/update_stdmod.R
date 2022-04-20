#'@title Update method for \code{std_selected} class output
#'
#'@description Update method and raise an error.
#'
#'@details This should be used only to update the call to [lm()],
#'          not to the call to [std_selected()] or [std_selected_boot()].
#'
#'@param object The output of the class \code{std_selected}.
#'@param formula. Changes to the formula.
#'@param ...  Optional arguments to be changed.
#'@param evaluate Whether the call will be evaluated.
#'
#'@examples
#' # See examples for std_selected.
#' @export

update.std_selected <- function(object, formula., ..., evaluate = TRUE) {
    # Adapted from update.default in stats
    if (is.null(call <- getCall(object))) {
        stop("need an object with call component")
      }
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.)) {
        call$formula <- update.formula(formula(object), formula.)
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
    lm_out_call$data <- object$lm_out_call$data
    if (evaluate) {
        # lm_out <- eval(lm_out_call, parent.frame())
        if (!is.null(object$std_selected_boot_call)) {
            new_call <- object$std_selected_boot_call
            new_call$lm_out <- lm_out_call
            out <- eval(new_call, parent.frame())
          } else {
            new_call <- object$std_selected_call
            new_call$lm_out <- lm_out_call
            out <- eval(new_call, parent.frame())
          }
      } else {
        return(call)
      }
  }
