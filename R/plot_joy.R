#' @title Joy plots
#'
#' @description
#' Plot small multiples of many overlapping density distributions,
#'     also known as a joy plot or stacked distributions.
#'
#' @param m  matrix or data.frame, where each column vector will have
#'      its own density polygon
#'
#' @param w matrix or data.frame, of same dimensions as \code{m}, with
#'      non-negative weights (in ecology, this might be species
#'      abundances)
#'
#' @param scaled logical, should polygons be scaled to maximum?
#'
#' @param excl   numeric, value(s) to exclude from density estimate
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
#' @param pts    optional vector of points to plot onto each row
#'
#' @param labcex expansion factor for y-axis text labels
#'
#' @param na.rm  logical, remove NA for density calculation?
#'
#' @param ...    further arguments passed to plot function
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
#' # synthetic example
#' set.seed(23)
#' x <- rnorm(99,10,1)
#' m <- data.frame(x = x,
#'                 y = x^1.2 + rnorm(99,0,1),
#'                 z = x^1.4 + rnorm(99,0,1))
#' plot_joy(m, ypad=1.05, labcex=1)        # allow overlap
#' plot_joy(m, yexp=1, ypad=1, labcex=1)   # forbid overlap
#' plot_joy(m, yexp=1, ypad=1, labcex=1, fcol=4:6) # color vector
#'
#' # real example
#' data(braun)
#' abu <- braun$spe              # species abundances
#' abu <- abu[colMeans(abu)>1]   # keep only most dominant species
#' env <- braun$env$bio1         # environment
#' occ <- abu
#' occ[occ>0] <- 1               # binary occurrences matrix
#' y   <- occ*env                # matrix of env values at occurrences
#' lab <- ' Annual Mean Temperature'
#' set_par(2)
#' plot_joy(y, w=NULL, xlab=lab) # unweighted
#' plot_joy(y, w=abu,  xlab=lab) # abundance-weighted
#'
#' @export
#' @rdname plot_joy
`plot_joy` <- function (m, w=NULL, scaled = TRUE, excl = 0,
                        fcol = '#00000080', lcol = '#000000',
                        lwid = 1, xinc = 0, xexp = 0.2,
                        yinc = ncol(m), yexp = 1.2, ypad = 1,
                        pts = NULL, labcex = 0.5, na.rm=TRUE, ...) {
     hasw <- !is.null(w)
     if (hasw){
          w <- as.matrix(w)
          if (!identical(dim(m), dim(w))) {
               stop('weights and data of unequal dimensions')
          }
     }
     m    <- as.matrix(m)
     nm   <- dimnames(m)[[2]]
     nspp <- dim(m)[2]
     xrng <- range(m[!(m %in% excl)], na.rm = na.rm)
     xmin <- xrng[1] - (diff(xrng) * xexp)
     xmax <- xrng[2] + (diff(xrng) * xexp)
     ht   <- nspp * ypad
     if (length(fcol) == 1)
          fcol <- rep(fcol, nspp)
     plot(1, xlim = c(xmin, xmax), ylim = c(0, ht), type = 'n',
          xaxs = 'i', yaxs = 'i', bty = 'l', yaxt = 'n', ylab = '',
          ...)
     for (j in nspp:1) {
          x  <- m[, j]
          if (hasw){
               wx <- w[, j][!(x %in% excl)]
               wx <- wx/sum(wx)
          } else {
               wx <- NULL
          }
          x  <-  x[!(x %in% excl)]
          if (length(unique(x)) == 1) {
               fuzz <- diff(xrng) * 0.01
               x  <- c(x - fuzz, x, x, x, x + fuzz)
               wx <- NULL
          }
          d <- stats::density(x, weights = wx, na.rm=na.rm)
          if (scaled)
               d$y <- d$y/(max(d$y))
          gx <- (j - 1) * (max(x) - min(x)) * xinc
          gy <- (j - 1) * (1/ht) * yinc
          zero <- min(yexp * d$y + gy)
          polygon(d$x + gx, yexp * d$y + gy, col = fcol[j])
          lines(d$x + gx, yexp * d$y + gy, col = lcol, lwd = lwid)
          lines(c(xmin, xmax), rep(zero, 2))
          if (!is.null(pts)) {
               points(pts[j], zero, pch = 16, col = 'black', cex=0.5)
          }
          mtext(nm[j], side = 2, line = 0.5, outer = F, at = zero,
                padj = 0, cex = labcex, las=1)
     }
}
