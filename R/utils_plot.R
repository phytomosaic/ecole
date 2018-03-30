#' @title Plotting utilities
#'
#' @description
#' Set "neat" graphical parameters suitable for publication, and also
#'     label panels on multi-panel figures.
#'
#' @param panels number of panels (1,2,4) to plot.
#'
#' @param pty a character specifying the type of plot region to be
#'     used; "s" generates a square plotting region and "m" generates
#'     the maximal plotting region.
#'
#' @param char alphabetic character to add as panel label.
#'
#' @param bold logical, should panel label be bold?
#'
#' @param ... further arguments passed to other methods (i.e.,
#'     \code{\link[graphics]{par}})
#'
#' @return
#' `set_par` sets graphical parameters; `add_label` adds panel label.
#'
#' @examples
#' N  <- 20
#' op <- set_par(4, pch=16)
#' plot(1:N, N:1)
#' add_label('a')
#' plot(sin(1:N), cos(N:1))
#' add_label('b')
#' plot(1:N, 1:N+rnorm(N,0,1), xlim=c(0,2*N), xlab='x', ylab='f(x)')
#' add_label('c')
#' plot(1:N, 1:N+rnorm(N,0,5), ylim=c(0,2*N), xlab='x', ylab='f(x)')
#' add_label('d')
#' par(op)
#'
#' @export
#' @rdname utils_plot
`set_par` <- function(panels=1, pty='s', ...){
     mgp <- c(2.5, 0.7, 0)    # margin for axis title, labels, line
     mar <- c(4, 4, 1.2, 1)   # plot margins
     oma <- c(0, 0, 0, 0)     # outer margins
     switch(as.character(panels),
            '4' = par(mfrow=c(2,2), mgp=mgp, mar=mar, pty=pty,
                      oma=oma, bty='L', las=1, cex.lab =1.2, ...),
            '3' = par(mfrow=c(1,3), mgp=mgp, mar=mar, pty=pty,
                      oma=oma, bty='L', las=1, cex.lab =1.4,
                      cex.axis=1.2, ...),
            '2' = par(mfrow=c(1,2), mgp=mgp, mar=mar, pty=pty,
                      oma=oma, bty='L', las=1, cex.axis=0.85, ...),
            '1' = par(mfrow=c(1,1), mgp=mgp, mar=mar, pty=pty,
                      oma=oma, bty='L', las=1, cex.axis=0.85, ...))
}
#' @export
#' @rdname utils_plot
`add_label` <- function(char='a', bold=FALSE, ...){
     if(!grepl("[[:alpha:]]",char)) stop('need alphabetic character')
     if(bold) font <- 2 else font <- 1
     text <- paste(LETTERS[letters==char], ')', sep='')
     mtext(text=text, side=3, adj=0, font=font, ...)
}
