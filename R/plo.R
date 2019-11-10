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
#' n <- 50
#' plo(runif(n), rnorm(n))
#' n <- 500
#' plo(runif(n), rnorm(n))
#' n <- 5000
#' plo(runif(n), rnorm(n))
#'
#' @rdname plo
#' @export
`plo` <- function(x, y=NULL, col=NULL, cex=0.7, pch=16, ...){
     n <- NROW(as.matrix(x))
     `num2hex` <- function(a) {
          hex <- unlist(strsplit("0123456789ABCDEF",split=""))
          return(paste(hex[(a-a%%16)/16+1],hex[a%%16+1],sep=""))
     }
     alfa <- if (n > 5000) 0.10 else (4-log10(n)) / 4
     if (is.null(col)) col <- paste0('#000000', num2hex(alfa * 255))
     plot(x=x, y=y, col=col, cex=cex, pch=pch, bty='L', las=1,
          mgp=c(2.3,0.7,0), tcl=-0.20, ...)
}
