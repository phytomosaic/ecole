#' @title Root mean square error (RMSE)
#'
#' @description Calculate root mean square error (RMSE) given
#' reference and predicted values
#'
#' @param y vector of reference values against which deviations
#'   are measured
#'
#' @param ypred vector of 'predicted' or 'sampled' values to evaluate,
#'   of same length as \code{y}
#'
#' @param ... further arguments passed to other methods
#'
#' @return Numeric value, which is the root mean square error (RMSE).
#'
#' @details This is one way to assess prediction accuracy.
#'
#' @examples
#' n <- 99
#' y <- c(1:n)
#' ypred <- x + rnorm(n, 0.1)
#' rmse(y, ypred)
#'
#' @seealso \link{https://en.wikipedia.org/wiki/Root-mean-square_deviation}
#'
#' @export
#' @rdname rmse
`rmse` <- function(y, ypred, ...){
     sqrt(mean((y-ypred)^2))
}
