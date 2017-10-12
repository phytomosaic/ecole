#' @title Joy plots
#'
#' @description
#' Plot small multiples of many overlapping density distributions,
#'     also known as a joy plot or stacked distributions.
#'
#' @param m  matrix or data.frame, where each column vector will have
#' its own density polygon
#'
#' @param fcol   polygon fill color(s)
#'
#' @param lcol   polygon line color
#'
#' @param lwid   polygon line width
#'
#' @param xinc   horizontal offset for each polygon (0 = aligned)
#'
#' @param xexp   x-axis expansion factor for entire plot
#'
#' @param yinc   vertical offset for ea polygon
#'
#' @param yexp   vertical expansion factor for each polygon
#'
#' @param ypad   vertical padding for entire plot
#'
#' @param scaled logical, should polygons be scaled to maximum?
#'
#' @param pts    optional vector of points to plot onto each row
#'
#' @param labcex expansion factor for y-axis text labels
#'
#' @param ...    further arguments passed to other methods
#'
#' @return
#' None, plots to device.
#'
#' @details
#' Plots one density polygon for each column in the data.frame or
#' matrix, resulting in potentially overlapping small multiples.
#' Polygons can be scaled vertically to their maximum, or else allowed
#' to vary freely.  Setting \code{yexp = 1}, \code{ypad = 1} and
#' \code{scaled = T} gives scaled polygons that fill each row with no
#' overlap.
#'
#' @examples
#' set.seed(23)
#' N <- 99
#' x <- rnorm(N,10,1)
#' y <- x^1.2 + rnorm(N,0,1)
#' z <- x^1.4 + rnorm(N,0,1)
#' m <- data.frame(x,y,z)
#' # allow overlap
#' plot_joy(m, ypad=1.05, labcex=1, xlab='Gradient X')
#' # forbid overlap
#' plot_joy(m, yexp = 1, ypad=1, labcex=1, xlab='Gradient X')
#' # color vector
#' plot_joy(m, fcol=c("#FF000080", "#A020F080", "#0000FF80"),
#'          yexp = 1, ypad=1, labcex=1, xlab='Gradient X')
#'
#' @export
#' @rdname plot_joy
`plot_joy` <- function(
     m,                  # matrix where each column will have its own density polygon
     fcol = rgb(0,0,0,0.5), # polygon fill color
     lcol = 'black',     # polygon line color
     lwid = 1,           # polygon line width
     xinc = 0,           # horiz offset for ea polygon (0 = aligned)
     xexp = 0.2,         # x-axis expansion factor for entire plot
     yinc = ncol(m),     # vert offset for ea polygon
     yexp = 1.2,         # vertical expansion factor for ea polygon
     ypad = 1,           # vertical padding for entire plot
     scaled = TRUE,      # should polygons be scaled to maximum?
     pts=NULL,
     labcex=0.5,
     ...){
     opar <- par()
     on.exit(par(opar))
     nspp <- dim(m)[2]
     xrng <- range(m, na.rm=T)
     xmin <- xrng[1]-(diff(xrng)*xexp)
     xmax <- xrng[2]+(diff(xrng)*xexp)
     ht   <- nspp*ypad
     if(length(fcol)==1) fcol <- rep(fcol, nspp)
     par( mfrow=c(1,1), las=1 )
     plot(1, xlim=c(xmin, xmax), ylim=c(0,ht),
          type='n', xaxs='i', yaxs='i', bty='l',
          yaxt='n', ylab='', las=1, ...)
     for(i in nspp:1){
          x  <- na.omit(m[,i])
          if(length(unique(x))==1){
               fuzz <- diff(xrng)*0.01
               x <- c(x-fuzz, x,x,x, x+fuzz)
          }
          d <- stats::density(x, na.rm=T)
          if(scaled){
               d$y <- d$y/(max(d$y))
          }
          gx <- (i-1)*(max(x)-min(x))*xinc     # horizontal offset
          gy <- (i-1)*(1/ht)*yinc              # vertical offset
          polygon(d$x+gx, yexp*d$y+gy, col=fcol[i])
          lines(  d$x+gx, yexp*d$y+gy, col=lcol, lwd=lwid)
          lines(c(xmin, xmax), rep(min(yexp*d$y+gy),2)  )
          if(!is.null(pts)){
               points(pts[i], min(yexp*d$y+gy),
                      pch=16, col='black', cex=0.5)
          }
          mtext(dimnames(m)[[2]][i], side=2, line=0.5, outer=F,
                at=min(yexp*d$y+gy), padj=0, cex=labcex)
     }
}