#' @title Log-log scatterplot
#'
#' @description
#' Scatterplot with logarithmic axes and gridlines.
#'
#' @param x,y
#' vectors of x- and y-axis values
#'
#' @param xlim,ylim
#' numeric vectors of length 2, giving x,y ranges on linear scale
#'
#' @param xlog,ylog
#' logical, should x,y axes be log-transformed? In case the linear
#'     scale is chosen, no gridlines are drawn.
#'
#' @param xbase,ybase
#' numeric, base of the logarithm of the respective axes. Ignored if
#'     linear axis is specified.
#'
#' @param grdcol
#' single value, color to be used for logarithmic gridlines
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' A plot object
#'
#' @details
#' Emulates classic log-log graph paper.  X and Y axes controlled
#'     independently.  Points may be added post-hoc.
#'
#' @author Petr Kiel
#'
#' @examples
#' # compare linear vs logarithmic axes
#' par(mfrow=c(1,2))
#' x <- seq(1, 1000, by=10)
#' plot(x, x, ylim=c(0,4000), las=1, bty='l')
#' points(x, x^0.8, col='blue')
#' points(x, x^1.2, col='red')
#' plot_loglog(x=log10(x), y=log10(x), xlab='X axis', ylab='Y axis',
#'             ylim=c(1, 10000), pch=16, col='#00000030')
#' points(log10(x), 0.8*log10(x), pch=16, col='#0000FF30')
#' points(log10(x), 1.2*log10(x), pch=16, col='#FF000030')
#'
#' # one axis log, one axis linear
#' x <- 1:1000
#' y <- dlnorm(x, meanlog=4)
#' yl <- c(0,0.012)
#' plot(x, y, ylim=yl, ylab='Pr density', bty='l', las=1)
#' plot_loglog(log10(x), y, ylog=FALSE, ylim=yl, xlim=c(0.1,1000),
#'      ylab='Pr den', pch=16, col='#00000030')
#' par(mfrow=c(1,1))
#'
#' @seealso
#' Lightly modified from http://www.petrkeil.com/?p=2701
#'
#' @rdname plot_loglog
#' @export
`plot_loglog` <- function(x, y, xlim=c(1,1000), ylim=c(1,1000),
                          xlog=TRUE, ylog=TRUE,
                          xbase=10, ybase=10,
                          grdcol='#00000020', ...){
     if(xlog){ # rounding the X-axis limits on the log scale
          xlim[1] <- floor(log(xlim[1], base=xbase))
          xlim[2] <- round(log(xlim[2], base=xbase))
          xbreaks <- xlim[1]:xlim[2]
     }
     if(ylog){ # rounding the Y-axis limits on the log scale
          ylim[1] <- floor(log(ylim[1], base=ybase))
          ylim[2] <- round(log(ylim[2], base=ybase))
          ybreaks <- ylim[1]:ylim[2]
     }
     plot(x,y,xlim,ylim,type='p',axes=FALSE,frame=TRUE,bty='l',...)
     if(xlog){ # plotting the X-axis tickmarks and grids
          for(x in xbase^xbreaks){
               subx <- log(seq(from=x, to=x*xbase, length=xbase),
                           base=xbase)
               abline(v=subx, col=grdcol)
          }
          axis(side=1, at=xbreaks, labels=xbase^xbreaks, tck=0.02)
     }
     else  axis(side=1, tck=0.02)
     if(ylog){ # plotting the Y-axis tickmarks and grids
          for(y in ybase^ybreaks){
               suby <- log(seq(from=y, to=y*ybase, length=ybase),
                           base=ybase)
               abline(h=suby, col=grdcol)
          }
          axis(side=2, at=ybreaks, labels=ybase^ybreaks,
               las=2, tck=0.02)
     }
     else axis(side=2, las=2, tck=0.02)
}
