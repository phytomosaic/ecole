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
#' @param x numeric, \emph{relative} horizontal position
#'
#' @param y numeric, \emph{relative} vertical position
#'
#' @param labels character vector of text to plot
#'
#' @param ... further arguments passed to other methods (i.e.,
#'     \code{\link[graphics]{par}})
#'
#' @return
#' `set_par` sets graphical parameters; `add_label` adds panel label;
#'     `add_text` adds text to the plotting area.
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
#' add_text(.1, .9, '+item1')
#' add_text(.5, .5, '+item2')
#' add_text(.9, .1, '+item3') # extends beyond plot area
#' par(op)
#'
#' @export
#' @rdname utils_plot
`set_par` <- function (panels = 1, pty = "s", ...) {
     mgp <- c(2.5, 0.7, 0)
     mar <- c(4, 4, 1.2, 1)
     oma <- c(0, 0, 0, 0)
     `auto_rowcol` <- function(n = panels) {
          if (n <= 3)
               c(1, n)
          else if (n <= 6)
               c(2, (n + 1)%/%2)
          else if (n <= 12)
               c(3, (n + 2)%/%3)
          else c(ceiling(n/(nr <- ceiling(sqrt(n)))), nr)
     }
     mfrow <- auto_rowcol()
     if(panels > 4) panels <- 4
     switch(as.character(panels),
            `4` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
                      oma = oma,  bty = "L", las = 1, cex.lab = 1.2,
                      ...),
            `3` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
                      oma = oma, bty = "L", las = 1, cex.lab = 1.4,
                      cex.axis = 1.2, ...),
            `2` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
                      oma = oma, bty = "L", las = 1, cex.axis = 0.85,
                      ...),
            `1` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
                      oma = oma, bty = "L", las = 1, cex.axis = 0.85,
                      ...))
}
#' @export
#' @rdname utils_plot
`add_label` <- function(char='a', bold=FALSE, ...){
     if(!grepl("[[:alpha:]]",char)) stop('need alphabetic character')
     if(bold) font <- 2 else font <- 1
     text <- paste(LETTERS[letters==char], ')', sep='')
     mtext(text=text, side=3, adj=0, font=font, ...)
}
#' @export
#' @rdname utils_plot
`add_text` <- function(x, y, labels, bold=FALSE, ...){
     if (bold)  font <- 2 else font <- 1
     text(x=graphics::grconvertX(x, from='npc', to='user'),
          y=graphics::grconvertY(y, from='npc', to='user'),
          labels=labels, adj=0, font=font, ...)
}
