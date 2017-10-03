#' @title Scatterplot with marginals
#'
#' @description
#' Scatterplot with marginal histograms, densities or orthogonal fit
#'     lines, useful for visualizing gradients on ordinations.
#'
#' @param d
#' array of data, where rows = SUs and cols = variables of interest
#'
#' @param x
#' character string for which column in \code{d} to place on x axis
#'
#' @param y
#' character string for which column in \code{d} to place on y axis
#'
#' @param over
#' character string for which column in \code{d} to overlay
#'
#' @param xlab
#' character string for label on x axis
#'
#' @param ylab
#' character string for label on y axis
#'
#' @param ovlab
#' character string for label on overlay axis when \code{marg='ortho'}
#'
#' @param marg
#' either \code{'hist'} for histograms,
#'        \code{'dens'} for density plots,
#'     or \code{'ortho'} for orthogonal fitlines
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' A plot object
#'
#' @examples
#' set.seed(23)
#' d <- data.frame(xx=rnorm(100, mean=3))
#' d$yy <- rnorm(100)*10 + d$x^2
#' d$zz <- d$y^2
#'
#' plot_marg(d=d, x='xx', y='yy', marg='hist')
#' plot_marg(d=d, x='xx', y='yy', marg='hist', breaks=22)
#' plot_marg(d=d, x='xx', y='yy', marg='dens')
#' plot_marg(d=d, x='xx', y='yy', over='zz', marg='ortho')
#'
#' @export
`plot_marg` <- function(d, x, y, over, xlab, ylab, ovlab,
                       marg='dens', alpha=1, ...){
     stopifnot(is.data.frame(d))
     if(!is.data.frame(d)) stop('\n`d` must be data.frame')
     # on.exit(par(mfrow=c(1,1)))
     if(missing(xlab)) xlab <- x
     if(missing(ylab)) ylab <- y
     x <- d[,x]
     y <- d[,y]
     zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
     graphics::layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
     fillcol <- grDevices::rgb(0,0,1, alpha=alpha)
     `scl` <- function(x){
          min(x, ...) + (0.5*diff(range(x, ...)))
     }
     if(marg=='dens'){
          xdens <- stats::density(x, ...)
          ydens <- stats::density(y, ...)
          top   <- max(c(xdens$y, ydens$y), ...)
          par(mar=c(2.8,2.8,1,1))
          plot(x, y, las=1, bty='l', ...)
          par(mar=c(0,2.8,1,1))
          plot(xdens$x, xdens$y, axes=FALSE, xlim=range(x), type='l')
          polygon(xdens$x, xdens$y, col='grey60', border='black')
          abline(v=median(x, ...), col='black', lwd=2)
          par(mar=c(2.8,0,1,1))
          plot(ydens$y, ydens$x, axes=FALSE, ylim=range(y), type='l')
          polygon(ydens$y, ydens$x, col='grey60', border='black')
          abline(h=median(y, ...), col='black', lwd=2)
          par(oma=c(2.8,2.8,0,0))
          mtext(xlab, side=1, line=1, outer=TRUE, adj=0, at=scl(x))
          mtext(ylab, side=2, line=1, outer=TRUE, adj=0, at=scl(y))
     }
     if(marg=='hist'){
          xhist <- hist(x, plot=FALSE, ...)
          yhist <- hist(y, plot=FALSE, ...)
          top   <- max(c(xhist$counts, yhist$counts), ...)
          par(mar=c(2.8,2.8,1,1))
          plot(x, y, las=1, bty='l', ...)
          par(mar=c(0,2.8,1,1))
          barplot(xhist$counts, axes=FALSE, ylim=c(0, top), space=0)
          par(mar=c(2.8,0,1,1))
          barplot(yhist$counts, axes=FALSE, xlim=c(0, top), space=0,
                  horiz=TRUE)
          par(oma=c(2.8,2.8,0,0))
          mtext(xlab, side=1, line=1, outer=TRUE, adj=0, at=scl(x))
          mtext(ylab, side=2, line=1, outer=TRUE, adj=0, at=scl(y))
     }
     if(marg=='ortho'){
          if(missing(over)) stop('\n`over` requires a variable name')
          if(missing(ovlab)) ovlab <- over
          par(mar=c(2.8,2.8,1,1))
          plot(x, y, las=1, bty='l', pch=21, bg=fillcol, ...)
          mtext(xlab, side=1, line=3, outer=F, adj=0, at=scl(x))
          mtext(ylab, side=2, line=3, outer=F, adj=0, at=scl(y))
          par(mar=c(0,2.8,1,1))
          pp   <- d[,over]
          taux <- round(stats::cor(x,pp,'na.or.complete','kendall'),2)
          taux <- paste0('Kendalls tau = ', taux)
          pca  <- stats::prcomp(~x+pp, ...)
          slp  <- with(pca, rotation[2,1] / rotation[1,1])
          int  <- with(pca, center[2] - slp*center[1])
          x0   <- min(x, ...)
          x1   <- max(x, ...)
          y0   <- (slp*x0)+int
          y1   <- (slp*x1)+int
          plot(x, pp, las=1, bty='l', cex=0.5, xaxt='n',
               pch=21, bg=fillcol, ...)
          lines(x=c(x0,x1), y=c(y0,y1), col='blue', lwd=2)
          text(grconvertX(0.01,"npc"), grconvertY(0.99, "npc"), taux,
               adj=c(0,1), cex=0.8)
          mtext(ovlab, side=2, line=3, outer=F, adj=0, at=scl(pp))
          par(mar=c(2.8,0,1,1))
          tauy <- round(stats::cor(y,pp,'na.or.complete','kendall'),2)
          tauy <- paste0('Kendalls tau = ', tauy)
          pca <- stats::prcomp(~y+pp, ...)
          slp <- with(pca, rotation[2,1] / rotation[1,1])
          int <- with(pca, center[2] - slp*center[1])
          x0 <- min(y, ...)
          x1 <- max(y, ...)
          y0 <- (slp*x0)+int
          y1 <- (slp*x1)+int
          plot(pp, y, las=1, bty='l', cex=0.5, yaxt='n',
               pch=21, bg=fillcol, ...)
          lines(x=c(y0,y1), y=c(x0,x1), col='blue', lwd=2)
          text(grconvertX(0.99,"npc"), grconvertY(0.02, "npc"), tauy,
               adj=c(1,0), cex=0.8)
          mtext(ovlab, side=1, line=3, outer=F, adj=0, at=scl(pp))
          par(oma=c(3,3,0,0))
     }
}