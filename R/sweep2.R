#' Similar to sweep, but will skip columns that are not numeric. 
#' It sweep only on the columns. 
#' @param x The data frame.
#' @param values A named vector with length equal to the number of columns
#'        in data.
#' @param FUN The function to be applied.
#' @noRd

sweep2 <- function(x, values, FUN = "-") {
    FUN <- match.fun(FUN)
    k <- ncol(x)
    n <- nrow(x)
    fct <- function(x, values, FUN) {
        if(is.numeric(x)) {
            out <- FUN(x, values)
          } else {
            out <- x
          }
        out
      }
    out <- mapply(fct, x, values, MoreArgs = list(FUN = FUN),
                  SIMPLIFY = FALSE)
    data.frame(out, stringsAsFactors = FALSE)
  }