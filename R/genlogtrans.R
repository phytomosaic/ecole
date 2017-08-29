#' Generalized log-transform
#'
#' Log transform properly handling zero and very small values
#'
#' @param x  vector of values to transform
#'
#' @return
#' Values on a modified log10(x+1) scale
#'
#' @details
#' Follows pg. 69 of McCune & Grace (2002).  Preserves original order
#'    of magnitudes while accounting for values = 0 and << 1.
#'
#' @examples
#' x  <- rnorm(99, 50, 20)
#' xt <- genlogtrans(x)
#' plot(x, xt)
#'
#' @family transformation functions
#'
#' @references
#' McCune, B., and J. B. Grace. 2002. Analysis of Ecological
#'     Communities. MjM Software, Gleneden Beach, Oregon, USA. 304 pp.
#' @export
`genlogtrans` <- function(x, ... ) {
     wasDataFrame <- is.vector(x)    # takes and returns a vector
     x <- as.matrix(x)               # convert to matrix
     minx  <- min( x[ which(x>0) ] ) # min of nonzero numbers
     const <- as.integer(log10(minx))# log10 of min is the constant
     d <- 10^const                   # 10 to the power of the constant
     x <- log10(x + d) - const       # produces NaNs
     if (wasDataFrame)               # return a vector
          x <- as.vector(x)
     x
}