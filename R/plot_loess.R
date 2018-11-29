#' @title Plotting locally-weighted regression line
#'
#' @description
#' Fit a polynomial surface determined by a numerical predictor using
#'     local fitting.
#'
#' @param x,y
#' vectors of x- and y-axis values.
#'
#' @param col
#' vector of point colors.
#'
#' @param lcol
#' vector of line colors.
#'
#' @param cex
#' numeric vector for scaling point size.
#'
#' @param pch
#' a vector of plotting characters or symbols: see
#'     \code{\link[graphics]{points}}.
#'
#' @param las
#' numeric, style of axis labels: see \code{\link[graphics]{par}}.
#'
#' @param bty
#' character string, determines type of box drawn around plot: see
#'     \code{\link[graphics]{par}}.
#'
#' @param ...
#' further arguments passed to functions.
#'
#' @return
#' A plot object.
#'
#' @details
#' Useful default settings for a scatterplot with locally-weighted
#'     regression line.
#'
#' @examples
#' set.seed(23)
#' x <- sin(1:55/pi)
#' y <- rnorm(55, 0, 0.45)+x
#' set_par(3)
#' plot_loess(x, y, span=0.10, main='Span = 0.10')
#' plot_loess(x, y, span=0.20, main='Span = 0.20')
#' plot_loess(x, y, span=0.40, main='Span = 0.40')
#'
#' @seealso \code{\link[stats]{loess}}
#'
#' @rdname plot_loess
#' @export
`plot_loess` <- function(x, y, col='#00000040', lcol='#FF0000BF',
                         cex=0.8, pch=16, las=1, bty='L', ...){
     ow <- getOption('warn')
     options(warn = -1)
     plot(x=x, y=y, col=col, pch=pch, las=las, bty=bty, cex=cex, ...)
     f    <- stats::loess(y ~ x, ...)
     fhat <- predict(f)
     o    <- order(x)
     lines(x[o], fhat[o], col=lcol, lwd=2)
     options(warn = ow)
}
