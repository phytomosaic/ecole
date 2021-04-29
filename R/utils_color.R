#' @title Color utilities
#'
#' @description
#' From given values, generate colors for plotting points or surfaces.
#'
#' @param x vector of values to evaluate
#'
#' @param n number of unique colors to be used in the palette
#'
#' @param pal optional specification for a color palette; defaults to
#'  \code{viridis::inferno} palette if missing
#'
#' @param zeroctr logical, should the color scale diverge to be symmetrical
#'   around zero?
#'
#' @param ngrid number of grid cells in the matrix of values plotted
#'  in \code{\link[graphics]{persp}}
#'
#' @param ... further arguments passed to other methods
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
#' f     <- surfcol(z, ngrid=ngrid, alpha=0.8)
#' persp(x, y, z, col=f, phi=30, theta=-30)
#'
#' # color points by numeric values
#' plot(x, y[1:30], col=colvec(x))
#'
#' # color points by factor values
#' trmt <- gl(3, 10, labels = c('Q', 'R', 'S'))
#' plot(x, y[1:30], col=colvec(rev(trmt)))
#'
#' # divergent colors centered on zero
#' set.seed(21)
#' x <- (rbeta(99, 0.5, 1) - 0.15) * 100
#' par(mfrow=c(1,3))
#' plot(x, col=colvec(x))
#' plot(x, col=colvec(x, zeroctr=TRUE))
#' plot(x, col=colvec(pseudo_log(x), zeroctr=TRUE))
#' par(mfrow=c(1,1))
#'
#' @export
#' @rdname utils_color
`colvec` <- function(x, n=99, pal, zeroctr=FALSE, ...) {
    if (isTRUE(zeroctr)) {
        if (missing(pal)) {
            pal <- c(
                '#0000FF','#0505FF','#0A0AFF','#0F0FFF','#1414FF',
                '#1A1AFF','#1F1FFF','#2424FF','#2929FF','#2E2EFF',
                '#3434FF','#3939FF','#3E3EFF','#4343FF','#4848FF',
                '#4E4EFF','#5353FF','#5858FF','#5D5DFF','#6262FF',
                '#6868FF','#6D6DFF','#7272FF','#7777FF','#7C7CFF',
                '#8282FF','#8787FF','#8C8CFF','#9191FF','#9696FF',
                '#9C9CFF','#A1A1FF','#A6A6FF','#ABABFF','#B0B0FF',
                '#B6B6FF','#BBBBFF','#C0C0FF','#C5C5FF','#CACAFF',
                '#D0D0FF','#D5D5FF','#DADAFF','#DFDFFF','#E4E4FF',
                '#EAEAFF','#EFEFFF','#F4F4FF','#F9F9FF','#FEFEFF',
                '#FFF9F9','#FFF4F4','#FFEFEF','#FFEAEA','#FFE4E4',
                '#FFDFDF','#FFDADA','#FFD5D5','#FFD0D0','#FFCACA',
                '#FFC5C5','#FFC0C0','#FFBBBB','#FFB6B6','#FFB0B0',
                '#FFABAB','#FFA6A6','#FFA1A1','#FF9C9C','#FF9696',
                '#FF9191','#FF8C8C','#FF8787','#FF8282','#FF7C7C',
                '#FF7777','#FF7272','#FF6D6D','#FF6868','#FF6262',
                '#FF5D5D','#FF5858','#FF5353','#FF4E4E','#FF4848',
                '#FF4343','#FF3E3E','#FF3939','#FF3434','#FF2E2E',
                '#FF2929','#FF2424','#FF1F1F','#FF1A1A','#FF1414',
                '#FF0F0F','#FF0A0A','#FF0505','#FF0000')
        }
        lim  <- max(abs(range(x, na.rm=TRUE)))
        brk  <- seq(-lim, lim, length.out=99)
        cutx <- cut(as.numeric(x), breaks=brk, include.lowest=TRUE)
        u    <- pal[cutx]
    } else {
        if (missing(pal)) {
            if (is.factor(x)) n <- nlevels(x)
            pal <- viridis::inferno(n=n, alpha=0.9, begin=0.1, end=0.85)
        }
        cutx <- cut(as.numeric(x), breaks=length(pal), include.lowest=TRUE)
        u    <- pal[cutx]
    }
    return(u)
}
#' @export
#' @rdname utils_color
`surfcol` <- function(x, ngrid, pal, ...) {
    if(missing(pal)){
        pal <- viridis::inferno(n=ngrid, alpha=0.9, begin=0.1, end=0.85)
    }
    xavg <- (x[-1, -1] +
                 x[-1, -(ncol(x) - 1)] +
                 x[-(nrow(x) -1), -1] +
                 x[-(nrow(x) -1), -(ncol(x) - 1)]) / 4
    pal[cut(xavg, breaks=length(pal), include.lowest = T)]
}
