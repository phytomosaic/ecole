#' Root mean square error (RMSE)
#'
#' Calculate root mean square error (RMSE) given reference and predicted
#'   values
#'
#' @param y  vector of reference values against which deviations
#'   are measured
#'
#' @param ypred  vector of 'predicted' or 'sampled' values to evaluate,
#'   of same length as \code{x}
#'
#' @return Numeric value, which is the root mean square error (RMSE).
#'
#' @details This is one way to assess prediction accuracy.
#'
#' @examples
#' n <- 99
#' x <- c(1:n)
#' y <- x + rnorm(n, 0.1)
#' rmse(x, y)
#'
#' @seealso \link{https://en.wikipedia.org/wiki/Root-mean-square_deviation}
#' @export
`rmse` <- function(y, ypred, ...){ sqrt(mean((y-ypred)^2)) }