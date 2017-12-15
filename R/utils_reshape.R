#' @title Reshape long to wide for perspective plots
#'
#' @description
#' Safely reshape 'long' data.frame to 'wide' format suitable for
#'     'persp', 'image', 'contour', 'filled.contour', 'heatmap', etc.
#'
#' @param data   data.frame of 3 columns, of which first two are x,y
#' locations and third column is z values
#'
#' @param ...    further arguments passed to other methods
#'
#' @return
#' List with components x$x and x$y for x and y axes, and x$z is a
#'      matrix containing the values to be plotted
#'
#' @details
#' Solves the dreaded "Error: increasing 'x' and 'y' values expected".
#'
#' @examples
#' set.seed(23)
#' d <- expand.grid(lon=seq(-125.5, -110.0, by=0.2),
#'                  lat=seq(40.5, 49.5, by=0.2))
#' d$z <- rnorm(nrow(d), 30, .2) * d$lat * d$lon
#' res <- reshape_p(d)
#' image(res)
#' contour(res, add=T, lwd=2, nlevels=12)
#' persp(res, theta=0, phi=45)
#' filled.contour(res, color.palette=heat.colors)
#' heatmap(res$z)
#'
#' @seealso
#' \code{\link[labdsv]{matrify}}
#'
#' @export
#' @rdname utils_reshape
`utils_reshape` <- function(data, ...){
     if(!is.data.frame(data)) stop('must be dataframe')
     if (ncol(data) != 3) stop('must have 3-column format')
     x <- data[, 1]
     y <- data[, 2]
     z <- data[, 3]
     fx <- as.factor(x)
     fy <- as.factor(y)
     m  <- matrix(NA, nrow=nlevels(fx), ncol=nlevels(fy),
                  dimnames=list(levels(fx), levels(fy)))
     m[cbind(fx, fy)] <- z
     list(x=sort(unique(x)), y=sort(unique(y)), z=m)
}