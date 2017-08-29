#' Arc-sine square root transform
#'
#' Transform appropriate for proportion data
#'
#' @param x  vector of values to transform
#'
#' @return
#' Values on the modified scale
#'
#' @details
#' Follows pg. 69 of McCune & Grace (2002).  Appropriate for
#'     vegetation cover and other proportion data.  Forces data to 0-1
#'     scale if any original values outside this range.
#'
#' @examples
#' x  <- rnorm(99, 50, 20)
#' xt <- asin_sqrt(x)
#' plot(x, xt)
#'
#' @family transformation functions
#'
#' @references
#' McCune, B., and J. B. Grace. 2002. Analysis of Ecological
#'     Communities. MjM Software, Gleneden Beach, Oregon, USA. 304 pp.
#' @export
`asin_sqrt` <- function(x, ... ) {
     wasDataFrame <- is.vector(x)       # takes and returns a vector
     x <- as.matrix(x)                  # convert to matrix
     if( any(x > 1) | any(x < 0) ) {
          cat("Range beyond 0-1, normalizing now")
          x <- (x-min(x, ...))/(max(x, ...)-min(x, ...)) # normalize
     }
     out <- (2/pi) * asin(sqrt(x))      # do the transform
     if (wasDataFrame)                  # return a vector
          out <- as.vector(out)
     return(out)
}