#' @title Plotting several kinds of regression line
#'
#' @description
#' Plot a regression line for linear, orthogonal, local-polynomial,
#'    kernel, cubic smoothing spline, or GAM regressions.
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
#' @param ci.col
#' confidence interval color.
#'
#' @param cex
#' numeric vector for scaling point size.
#'
#' @param pch
#' a vector of plotting characters or symbols: see
#'     \code{\link[graphics]{points}}.
#'
#' @param xlab,ylab
#' character strings for axis labels.
#'
#' @param args.func
#' further arguments passed to the regression function.
#'
#' @param interval
#' type of interval calculation, for linear model only.
#'
#' @param ...
#' further arguments passed to \code{plot}.
#'
#' @return
#' A plot object.
#'
#' @details
#' Plots a scatterplot with a regression line, useful for quick
#'     exploratory analysis.
#'
#' @examples
#' set.seed(23)
#' n <- 55
#' x <- sin(1:n/pi)
#' y <- runif(n, 0, 0.75) + x
#' x[12] <- NA  # handles NA
#' set_par(6)
#' plot_lm(x, y, main='Linear', interval='confidence')
#' plot_ortho(x, y, main='Orthogonal') # not identical to linear!
#' plot_loess(x, y,  args.func=list(span=0.2), main='Loess')
#' plot_kernel(x, y, args.func=list(bandwidth=0.2), main='Kernel')
#' plot_spline(x, y, args.func=list(df=11), main='Spline')
#' plot_gam(x, y, args.func=list(gamma=0.5), main='GAM')
#'
#' @seealso \code{\link[stats]{lm}},
#'    \code{\link[stats]{prcomp}},
#'    \code{\link[stats]{loess}},
#'    \code{\link[stats]{ksmooth}},
#'    \code{\link[stats]{smooth.spline}}, and
#'    \code{\link[mgcv]{gam}} for the underlying functions.
#'
#' @aliases plot_lm
#' @aliases plot_ortho
#' @aliases plot_loess
#' @aliases plot_kernel
#' @aliases plot_spline
#' @aliases plot_gam
#'
#' @rdname plot_lm
#' @export
`plot_lm` <- function (x, y, col = '#00000040', lcol = '#FF0000BF',
                       ci.col = '#ffd800', cex = 0.8, pch = 16, xlab = NULL,
                       ylab = NULL, args.func = list(),
                       interval = c('none', 'confidence', 'prediction'), ...) {
        if (is.null(xlab))
                xlab <- deparse(substitute(x))
        if (is.null(ylab))
                ylab <- deparse(substitute(y))
        plot(x = x, y = y, col = col, pch = pch,
             cex = cex, xlab = xlab, ylab = ylab, ...)
        f <- do.call(stats::lm, c(list(formula = y ~ x), args.func))
        if (length(interval) > 1) interval <- interval[1]
        if (interval %in% c('confidence','prediction')) {
                xrng <- range(x, na.rm=TRUE)
                xpad <- diff(xrng) * 0.05
                s    <- data.frame(seq(xrng[1] - xpad, xrng[2] + xpad, len=499))
                dimnames(s)[[2]] <- deparse(substitute(x))
                p    <- predict(f, newdata=s, interval=interval)
                lines(s[,1], p[,2], col=paste0(ci.col,'DD'), lwd=2)
                lines(s[,1], p[,3], col=paste0(ci.col,'DD'), lwd=2)
                polygon(c(s[,1],rev(s[,1])), c(p[,2],rev(p[,3])),
                        col=paste0(ci.col,'70'), border=NA)
        }
        abline(f, col = lcol, lwd = 2)
}
#' @rdname plot_lm
#' @export
`plot_ortho` <- function (x, y, col = '#00000040', lcol = '#FF0000BF',
                          cex = 0.8, pch = 16, xlab = NULL, ylab = NULL,
                          args.func = list(), ...) {
        if (is.null(xlab))
                xlab <- deparse(substitute(x))
        if (is.null(ylab))
                ylab <- deparse(substitute(y))
        plot(x = x, y = y, col = col, pch = pch,
             cex = cex, xlab = xlab, ylab = ylab, ...)
        pca <- do.call(stats::prcomp,
                       c(list(formula = ~ x + y,
                              retx   = FALSE,
                              center = TRUE,
                              scale. = FALSE,
                              rank.  = 2,
                              na.action=na.omit),
                         args.func))
        slp <- with(pca, rotation[2, 1]/rotation[1, 1])
        int <- with(pca, center[2] - slp * center[1])
        x0  <- min(x, na.rm = TRUE)
        x1  <- max(x, na.rm = TRUE)
        y0  <- (slp * x0) + int
        y1  <- (slp * x1) + int
        lines(x = c(x0, x1), y = c(y0, y1), col = lcol, lwd = 2)
}
#' @rdname plot_lm
#' @export
`plot_loess` <- function (x, y, col = '#00000040', lcol = '#FF0000BF',
                          cex = 0.8, pch = 16, xlab = NULL, ylab = NULL,
                          args.func = list(), ...) {
        if (is.null(xlab))
                xlab <- deparse(substitute(x))
        if (is.null(ylab))
                ylab <- deparse(substitute(y))
        plot(x = x, y = y, col = col, pch = pch,
             cex = cex, xlab = xlab, ylab = ylab, ...)
        f <- do.call(stats::loess,
                     c(list(formula = y ~ x), args.func))
        rng  <- range(x, na.rm=TRUE)
        xs   <- seq(rng[1],rng[2],len=100)
        fhat <- predict(f, data.frame(x=xs))
        lines(xs, fhat, col = lcol, lwd = 2)
}
#' @rdname plot_lm
#' @export
`plot_kernel` <- function (x, y, col = '#00000040', lcol = '#FF0000BF',
                           cex = 0.8, pch = 16, xlab = NULL, ylab = NULL,
                           args.func = list(), ...) {
        if (is.null(xlab))
                xlab <- deparse(substitute(x))
        if (is.null(ylab))
                ylab <- deparse(substitute(y))
        plot(x = x, y = y, col = col, pch = pch,
             cex = cex, xlab = xlab, ylab = ylab, ...)
        if(!('bandwidth' %in% names(args.func))) {
                args.func <- c(bandwidth=diff(range(x,na.rm=T))/0.10,
                               args.func)
        }
        isna <- is.na(x) | is.na(y)
        f <- do.call(stats::ksmooth,
                     c(list(x[!isna], y[!isna], kernel='normal'),
                       args.func))
        lines(f, col = lcol, lwd = 2)
}
#' @rdname plot_lm
#' @export
`plot_spline` <- function (x, y, col = '#00000040', lcol = '#FF0000BF',
                           cex = 0.8, pch = 16, xlab = NULL, ylab = NULL,
                           args.func = list(), ...) {
        if (is.null(xlab))
                xlab <- deparse(substitute(x))
        if (is.null(ylab))
                ylab <- deparse(substitute(y))
        plot(x = x, y = y, col = col, pch = pch,
             cex = cex, xlab = xlab, ylab = ylab, ...)
        isna <- is.na(x) | is.na(y)
        f <- do.call(stats::smooth.spline,
                     c(list(x[!isna], y[!isna], keep.data=F),
                       args.func))
        lines(f, col = lcol, lwd = 2)
}
#' @rdname plot_lm
#' @export
#' @import mgcv
`plot_gam` <- function (x, y, col = '#00000040', lcol = '#FF0000BF',
                        cex = 0.8, pch = 16, xlab = NULL, ylab = NULL,
                        args.func = list(), ...) {
        if (is.null(xlab))
                xlab <- deparse(substitute(x))
        if (is.null(ylab))
                ylab <- deparse(substitute(y))
        plot(x = x, y = y, col = col, pch = pch,
             cex = cex, xlab = xlab, ylab = ylab, ...)
        isna <- is.na(x) | is.na(y)
        f    <- do.call(mgcv::gam, c(list(formula = y ~ s(x)),
                                     args.func))
        rng  <- range(x, na.rm = TRUE)
        xs   <- seq(rng[1], rng[2], len = 100)
        fhat <- predict(f, data.frame(x = xs))
        lines(xs, fhat, col = lcol, lwd = 2)
        # abline(v=fhat[which.max(fhat)], col=4, lwd=2) # <-- GAM PEAK
}
