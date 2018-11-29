#' @title Simple scatterplot with useful defaults
#'
#' @description
#' Scatterplot with sensible default settings that permit
#'     overplotting many points.
#'
#' @param x,y
#' vectors of x- and y-axis values.
#'
#' @param col
#' vectors of point colors.
#'
#' @param cex
#' numeric vector for scaling point size.
#'
#' @param pch
#' a vector of plotting characters or symbols: see
#'     \code{\link[graphics]{points}}.
#'
#' @param ...
#' further arguments passed to \code{\link[graphics]{plot}}.
#'
#' @return
#' A plot object.
#'
#' @details
#' Useful default settings for a simple scatterplot, closed circles
#'     with transparent black color that permits overplotting.
#'     Follows \code{graphics::plot.default}.
#'
#' @examples
#' set.seed(23)
#' x <- runif(9999)
#' y <- rnorm(9999)
#' plo(x, y, las=2, bty='l')
#'
#' @rdname plo
#' @export
`plo` <- function(x, y=NULL, col='#00000020', cex=0.5, pch=16, ...){
     plot(x=x, y=y, col=col, cex=cex, pch=pch, ...)
}
