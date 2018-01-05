#' @title Color utilities
#'
#' @description
#' From given values, generate colors for plotting points or surfaces.
#'
#' @param x vector of values to evaluate
#' @param n number of unique colors to be used in the palette
#' @param alpha alpha transparency in [0,1]
#' @param begin hue in [0,1] at which the viridis colormap begins
#' @param end hue in [0,1] at which the viridis colormap ends
#' @param dir order of colors in the scale; if 1, the default, colors
#'  are ordered from darkest to lightest; if -1, the order of colors
#'  is reversed
#' @param pal optional specification for a color palette; defaults to
#'  \code{viridis::inferno} palette if missing
#' @param ... further arguments passed to other methods
#' @param ngrid number of grid cells in the matrix of values plotted
#' in \code{\link[graphics]{persp}}
#'
#' @return
#' Vector or matrix of color values
#'
#' @examples
#' # generate data
#' x <- seq(-1.95, 1.95, length = 30)
#' y <- seq(-1.95, 1.95, length = 35)
#' z <- outer(x, y, function(a, b) a*b^2)
#'
#' # color surface of perspective plot by 'z' value
#' ngrid <- prod(dim(z))
#' f <- surfcol(z, ngrid=ngrid, alpha=0.8)
#' persp(x, y, z, col=f, phi=30, theta=-30)
#'
#' # color points by numeric value
#' plot(x, y[1:30], col=colvec(x), pch=16)
#'
#' # color points by factor value
#' trmt <- gl(3, 10, labels = c('Q', 'R', 'S'))
#' plot(x, y[1:30], col=colvec(rev(trmt)), pch=16)
#'
#' @export
#' @rdname utils_color
`colvec` <- function(x, n=99, alpha=0.6, begin=0.2, end=0.9, dir=1,
                     pal, ...) {
     if(is.factor(x)){
          n <- nlevels(x)
     }
     if(!require(viridis) & missing(pal)){
          pal <- grDevices::rainbow(n=n, alpha=alpha, ...)
     } else {
          if(missing(pal)){
               pal <- viridis::inferno(n=n, alpha=alpha, begin=begin,
                                       end=end, direction=dir)
          }
     }
     pal[cut(as.numeric(x), breaks=length(pal), include.lowest=TRUE)]
}
#' @export
#' @rdname utils_color
`surfcol` <- function(x, ngrid, alpha=0.6, begin=0.2, end=0.9,  dir=1,
                      pal, ...){
     if(!require(viridis) & missing(pal)){
          pal <- grDevices::rainbow(n=ngrid, alpha=alpha, ...)
     } else {
          if(missing(pal)){
               pal <- viridis::inferno(n=ngrid, alpha=alpha,
                                       begin=begin, end=end,
                                       direction=dir)
          }
     }
     xavg <- (x[-1, -1] +
                   x[-1, -(ncol(x) - 1)] +
                   x[-(nrow(x) -1), -1] +
                   x[-(nrow(x) -1), -(ncol(x) - 1)]) / 4
     pal[cut(xavg, breaks=length(pal), include.lowest = T)]
}
