#' @title Determine all duplicate elements
#
#' @description Determines which elements of a vector or data frame
#'     are duplicates of other elements, and returns either a logical
#'     vector indicating which elements (rows) are duplicates, or the
#'     duplicated elements (rows) themselves.
#'
#' @param x a vector or a data frame or an array or NULL.
#'
#' @param xret logical, default \code{FALSE} returns a logical vector
#'      indicating which rows are duplicates, \code{TRUE} returns the
#'      duplicated elements.
#'
#' @param ... additional arguments passed to
#'      \code{\link[base]{duplicated}}.
#'
#' @return
#' Numeric vector indexing rows (sample units) in \code{spe1} which
#'     are nearest compositional neighbors of \code{spe2}. The order
#'     corresponds to row order in \code{spe2}.
#'
#' @details
#' Behavior nearly identical to \code{\link[base]{duplicated}}, except
#'     returns ALL duplicated elements, not simply the first.
#'
#' @examples
#' x <- c('a','b','c','c','c')
#' y <- data.frame(x, y=c(1,2,4,4,5))
#' z <- as.matrix(y)
#' duplicates(x) # duplicate_s_
#' duplicated(x) # compare with duplicate_d_
#' duplicates(y)
#' duplicated(y)
#' duplicates(z)
#' duplicated(z)
#'
#' @seealso \code{\link[base]{duplicated}}, and
#'      https://stackoverflow.com/questions/7854433/.
#'
#' @export
#' @rdname duplicates
`duplicates` <- function(x, xret=FALSE, ...){
     i <- duplicated(x, ...) | duplicated(x, fromLast=TRUE, ...)
     if(xret){
          if(is.vector(x)){
               x[i]
          } else {
               x[i,,]
          }
     } else {
          i
     }
}
