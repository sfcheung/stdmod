#' @title The 'add1' Method for a 'std_selected' Class Object
#'
#' @description Intercept the [add1()] method and raise an error.
#'
#' @details [add1()] should be not be used after the output of [lm()] is
#'  processed by [std_selected()] or [std_selected_boot()].
#'
#' @param object The output of [std_selected()] or [std_selected_boot()].
#' @param ...  Optional arguments. They will be ignored.
#'
#' @export

add1.std_selected <- function(object, ...) {
    stop(paste0("add1 should be used on the lm() call first and then",
                "call std_selected() or std_selected_boot() again."))
  }
