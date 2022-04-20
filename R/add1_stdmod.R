#'@title add1 method for \code{std_selected} class output
#'
#'@description Intercept the add1 method and raise an error.
#'
#'@details [add1()] should be not be used after an `lm` object is processed by
#'  [std_selected()] or [std_selected_boot()].
#'
#'@param object The output of the class \code{std_selected}.
#'@param ...  Optional arguments. They will be ignored.
#'
#'@examples
#' # See examples for std_selected.
#' @export

add1.std_selected <- function(object, ...) {
    stop(paste0("add1 should be used on the ",
                "call std_selected or std_selected_boot again."))
  }
