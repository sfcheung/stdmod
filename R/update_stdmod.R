#'@title update method for \code{std_selected} class output
#'
#'@description Intercept the update method and raise an error.
#'
#'@details [update()] should be done in the call to [lm()] before calling
#'  [std_selected()] or [std_selected_boot()].
#'
#'@param object The output of the class \code{std_selected}.
#'@param ...  Optional arguments. They will be ignored.
#'
#'@examples
#' # See examples for std_selected.
#' @export

update.std_selected <- function(object, ...) {
    stop(paste0("If the model needs to be updated, do it in lm and then ",
                "call std_selected or std_selected_boot again."))
  }
