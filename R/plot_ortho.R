#' @title Scatterplot with orthogonal fit line
#'
#' @description
#' Scatterplot with orthogonal fit line and annotations.  Also
#' permits converting from points to interpoint Euclidean distances.
#'
#' @param x
#' vector of x-axis values
#'
#' @param y
#' vector of y-axis values
#'
#' @param dist
#' logical, convert values to interpoint Euclidean distances?
#'
#' @param col
#' single value or vector of point colors
#'
#' @param ann
#' logical, should annotations be added to top of plot?
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' A plot object
#'
#' @details
#' Uses \code{\link[stats]{prcomp}} to fit orthogonal line. Data are
#' first centered but not scaled for PCA step, therefore, consider
#' carefully whether unscaled data are meaningful in your use case.
#' Plot aspect is fixed at \code{asp=1}.
#'
#' Converting values to interpoint distances may be useful for
#' comparing interpoint distances in e.g., ordination scores.
#'
#' @examples
#' set.seed(23)
#' N  <- 99
#' x  <- runif(N,0,1)
#' y1 <- x + rnorm(N, 0, .1)
#' y2 <- x + rnorm(N, 0, .5)
#' y3 <- rnorm(N, 0, x*.3)
#' op <- par(mfrow=c(2,2))
#' plot_ortho(x, y1)
#' plot_ortho(x, y2)
#' plot_ortho(x, y3)
#' plot_ortho(y3, x)
#' par(op)
#'
#' @rdname plot_ortho
#' @export
`plot_ortho` <- function(x, y, dist=FALSE, col, ann=TRUE, ...) {
     if(missing(col)){
          col <- '#00000080'
     }
     if(dist){
          x <- as.vector(vegan::vegdist(x, 'euc', na.rm=TRUE))
          y <- as.vector(vegan::vegdist(y, 'euc', na.rm=TRUE))
          cat('\nConverting values to interpoint distances')
     }
     if(!is.vector(x) | !is.vector(y)){
          stop('x and y must be vectors')
     }
     pca <- stats::prcomp(~x+y, center=TRUE, scale.=FALSE)
     slp <- with(pca, rotation[2,1] / rotation[1,1])
     int <- with(pca, center[2] - slp*center[1])
     x0  <- min(x, na.rm=TRUE)
     x1  <- max(x, na.rm=TRUE)
     y0  <- (slp*x0)+int
     y1  <- (slp*x1)+int
     plot(x, y, pch=16, col=col, las=1, bty='l', asp=1, ...)
     lines(x=c(x0,x1), y=c(y0,y1), col='grey60', lwd=2)
     if(ann){
          vexp <- signif(((pca$sdev)^2/sum(pca$sdev^2))[1],2)*100
          z1  <- paste0('Var. expl. = ', vexp,'%')
          z2  <- paste0('Slope = ', signif(slp,2))
          mtext(z1, 3, 0, outer=F, at=grconvertX(.99,'npc'), adj=1, cex = 0.7)
          mtext(z2, 3, 0, outer=F, at=grconvertX(.01,'npc'), adj=0, cex = 0.7)
     }
}
