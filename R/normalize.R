#' Normalize
#'
#' Normalize (relativize) data to 0-1 scale
#'
#' @param x  vector of values to transform
#'
#' @return
#' Values on the modified scale
#'
#' @details
#' Relativizes data to 0-1 scale based on min/max values.
#'
#' @examples
#' x  <- rnorm(99, 50, 20)
#' xt <- normalize(x)
#' plot(x, xt)
#'
#' @family transformation functions
#'
#' @references
#' McCune, B., and J. B. Grace. 2002. Analysis of Ecological
#'     Communities. MjM Software, Gleneden Beach, Oregon, USA. 304 pp.
#' @export
`normalize` <- function(x, ...){
     (x - min(x, ...)) / (max(x, ...) - min(x, ...))
}