#' Sample standard error
#'
#' Calculate sample standard error for a vector.
#'
#' @param x  vector of values to evaluate
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' Numeric value which is the sample standard error
#'
#' @details
#' Uses \emph{n-1} in denominator to correct for small-sample bias.
#'
#' @examples
#' x <- c(1:99)
#' sem(x)
#'
#' @seealso \code{\link[base]{sd}}
#' @export
`sem` <- function(x, ...) {
     sd(x, ...) / sqrt(length(na.omit(x))-1)
}
