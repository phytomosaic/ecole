#' @title Plotting utilities
#'
#' @description
#' Set 'neat' graphical parameters suitable for publication, and also
#'     label panels on multi-panel figures.
#'
#' @param panels number of panels (e.g., 1,2,4,9) to plot.
#'
#' @param CEX character expansion factor (default = 1).
#'
#' @param char alphabetic character to add as panel label.
#'
#' @param x numeric, \emph{relative} horizontal position
#'
#' @param y numeric, \emph{relative} vertical position
#'
#' @param labels character vector of text to plot
#'
#' @param ... further arguments passed to \code{\link[graphics]{par}}
#'
#' @return
#' `set_par` sets graphical parameters, and `set_par_mercury` does so
#'     in a retro style; `add_label` adds a panel label to plot
#'     margins; `add_text` adds text to the plotting area.
#'
#' @examples
#' ### standard usage
#' N  <- 99
#' op <- set_par()
#' plot(1:N, 1:N+rnorm(N,0,5), ylim=c(0,2*N), xlab='x', ylab='f(x)')
#' add_text(.1, .9, '+item1')
#' add_text(.5, .5, '+item2')
#' par(op)
#'
#' ### is today 1963 ??
#' x <- runif(99)
#' y <- sin(6 * pi * x) + rnorm(99, sd=0.1)
#' op <- set_par_mercury()
#' plot(x, y, cex=0.7, asp=0.25, ylab='f(x)')
#' add_text(.1, .9, 'SINUSOIDAL', cex=0.9)
#' par(op)
#'
#' ### Project Mercury space missions
#' ###  data: https://www.nature.com/articles/s41526-018-0040-5/tables/3
#' d <- strsplit(c(
#'         'MR-3	5/5/1961	0.2577778	0.08444444	0	A.B. Shepard',
#'         'MR-4	7/21/1961	0.2602778	0.08333333	0	V.I. Grissom',
#'         'MA-6	2/20/1962	4.923056	4.633333	3	J.H. Glenn',
#'         'MA-7	5/24/1962	4.934722	4.65	3	M.S. Carpenter',
#'         'MA-8	10/3/1962	9.219722	8.939444	6	W.M. Schirra',
#'         'MA-9	5/15/1963	34.33028	34.05833	22	L.G. Cooper'), "\t")
#'
#' d <- data.frame(do.call(rbind, d), stringsAsFactors=F)
#' dimnames(d)[[2]] <- c('mission','date','duration',
#'                       'weightless_hr','orbits','pilot')
#' d$date <- as.Date(d$date, format="%m/%d/%Y")
#' op <- set_par_mercury(3, cex.axis=0.9, mar=c(4,4,2,1))
#' plot(d$date, d$orbits, ylim=c(-0.1,25), ylab='Earth orbits',
#'      xlab='Date', cex=0.7)
#' plot(d$date, d$duration, ylim=c(-0.1,35),  ylab='Duration (h)',
#'      xlab='Date', cex=0.7)
#' plot(d$date, d$weightless_hr, ylim=c(-0.1,35),
#'      ylab='Duration weightless (h)', xlab='Date', cex=0.7)
#' mtext('Project Mercury missions', side=3, line=2, cex=1.5, at=-3900)
#' par(op)



# `set_par` <- function (panels = 1, pty = 's', ...) {
#         mgp <- c(2, 0.4, 0)
#         mar <- c(4, 4, 0.5, 0.5)
#         oma <- c(0, 0, 0, 0)
#         `auto_rowcol` <- function(n = panels) {
#                 if (n <= 3)
#                         c(1, n)
#                 else if (n <= 6)
#                         c(2, (n + 1)%/%2)
#                 else if (n <= 12)
#                         c(3, (n + 2)%/%3)
#                 else c(ceiling(n/(nr <- ceiling(sqrt(n)))), nr)
#         }
#         mfrow <- auto_rowcol()
#         if(panels > 4) panels <- 4
#         switch(as.character(panels),
#                `4` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
#                          oma = oma, bty = 'L', las = 1, cex.lab = 1.2,
#                          tcl=-0.2, ...),
#                `3` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
#                          oma = oma, bty = 'L', las = 1, cex.lab = 1.4,
#                          cex.axis = 1.2, tcl=-0.2, ...),
#                `2` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
#                          oma = oma, bty = 'L', las = 1, cex.axis = 0.85,
#                          tcl=-0.2, ...),
#                `1` = par(mfrow = mfrow, mgp = mgp, mar = mar, pty = pty,
#                          oma = oma, bty = 'L', las = 1, cex.axis = 0.85,
#                          tcl=-0.2, ...))
# }


#' @export
#' @rdname utils_plot
`set_par` <- function (panels = NULL, CEX=1.0, ...) {
        z  <- list(...)
        nm <- names(z)  # NULL if no additional arguments given
        `f` <- function(x, val) { if (!(x %in% nm)) z[[x]] <<- val }
        f('pch', 16)
        f('mar', c(4, 4, 1, 0.5))
        f('oma', c(0.1, 0.1, 0.1, 0.1))
        f('bty', 'L')
        f('pty', 's')
        f('las', 1)
        f('mgp', c(2, 0.4, 0))
        f('tcl', -0.2)
        if(!is.null(panels)) {
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
                f('mfrow', mfrow)
                if (panels > 4) panels <- 4
                switch(as.character(panels),
                       `4` = f('cex.lab', 1.2),
                       `3` = { f('cex.axis', 1.2); f('cex.lab', 1.4) },
                       `2` = f('cex.axis', 0.85),
                       `1` = f('cex.axis', 0.85))
        }
        if (length(names(z)) > 1) z <- z[order(names(z))]
        invisible(par(z)) # set parameters, while keeping defaults
}

#' @export
#' @rdname utils_plot
`set_par_mercury` <- function(panels=NULL, CEX=1.0, ...) {
        `check_has_font` <- function() {   ###   unexported   ###
                if(Sys.info()[1] != 'Windows')
                        stop('Currently only supported on windows devices')
                ### import, install and register the font
                ###   from: https://webonastick.com/fonts/routed-gothic/
                require(extrafont)
                has <- 'Routed Gothic' %in% extrafont::fonttable()$FamilyName
                if (!has) {
                        message('Downloading `Routed Gothic` font now...\n')
                        ### 1. download the font if it doesnt exist
                        g <- 'https://github.com/dse/routed-gothic/blob/master/'
                        g <- paste0(g, 'dist/ttf/')
                        f1 <- tempfile(pattern = 'font_1_to_install__')
                        download.file(url=paste0(g,'routed-gothic.ttf'),
                                      destfile=f1)
                        f2 <- tempfile(pattern = 'font_2_to_install__')
                        download.file(url=paste0(g,'routed-gothic-italic.ttf'),
                                      destfile=f2)
                        msg <- '\nNow manually install these two fonts'
                        msg <- paste0(msg,'on your system:\n')
                        message(paste0(msg, f1, '\nand\n', f2, '\n'))
                        opendir <- function(dir = tempdir()){
                                if (.Platform['OS.type'] == 'windows'){
                                        shell.exec(dir)
                                } else {
                                        system(paste(Sys.getenv('R_BROWSER'), dir))
                                }
                        }
                        opendir()
                        ### 2. pause to manually install
                        msg <- paste0('\n\nHave you installed these files?')
                        msg <- paste0(msg, '\n\nPress y or n to continue...\n\n')
                        resp <- readline(paste0(msg))
                        if (!grepl('y', resp, ignore.case=T)) {
                                message('Must manually install fonts on system!')
                                return(invisible())
                        } else {
                                ### 3. import it
                                extrafont::font_import(pattern='routed-gothic',
                                                       prompt=F)
                                ### 4. register it for multiple devices
                                extrafont::loadfonts(device='win')
                                extrafont::loadfonts(device='pdf')
                                extrafont::loadfonts(device='postscript')
                                ### 5. translate to windows and pdf devices
                                windowsFonts(sans='Routed Gothic')
                                pdfFonts(sans='Routed Gothic')
                        }
                        message('Fonts successfully loaded; more details at:')
                        message(paste0('https://cran.r-project.org/web/',
                                       'packages/extrafont/README.html'))
                        return(TRUE)
                } else {
                        return(TRUE)
                }
        }
        stopifnot(isTRUE(check_has_font()))
        z  <- list(...)
        nm <- names(z)  # NULL if no additional arguments given
        `f` <- function(x, val) { if (!(x %in% nm)) z[[x]] <<- val }
        f('cex.axis', CEX*1)
        f('cex.lab',  CEX*1.3)
        f('cex.main', CEX*1.3)
        f('pch', 16)
        f('mar', c(3, 3, 1, 0.5))
        f('oma', c(0.1, 0.1, 0.1, 0.1))
        f('family', 'Routed Gothic')
        f('bty', 'L')
        f('pty', 's')
        f('las', 1)
        f('mgp', c(CEX+0.5, 0.2, 0))
        f('tcl', 0.2)
        if(!is.null(panels)) {
                `auto_rowcol` <- function(n = panels) {
                        if (n <= 3)
                                c(1, n)
                        else if (n <= 6)
                                c(2, (n + 1)%/%2)
                        else if (n <= 12)
                                c(3, (n + 2)%/%3)
                        else c(ceiling(n/(nr <- ceiling(sqrt(n)))), nr)
                }
                f('mfrow', auto_rowcol())
                if (panels > 4) panels <- 4
                switch(as.character(panels),
                       `4` = f('cex.lab',  CEX*1.4),
                       `3` = { f('cex.axis', 1.0); f('cex.lab', 1.4) },
                       `2` = f('cex.axis',  CEX*0.9),
                       `1` = f('cex.axis',  CEX*0.9))
        }
        if (length(names(z)) > 1) z <- z[order(names(z))]
        invisible(par(z)) # set parameters, while keeping defaults
}
#' @export
#' @rdname utils_plot
`add_label` <- function(char='a', ...){
        if(!grepl('[[:alpha:]]',char)) stop('need alphabet char')
        text <- paste(LETTERS[letters==char], ')', sep='')
        mtext(text=text, side=3, adj=0, ...)
}
#' @export
#' @rdname utils_plot
`add_text` <- function(x, y, labels, ...){
        text(x=graphics::grconvertX(x, from='npc', to='user'),
             y=graphics::grconvertY(y, from='npc', to='user'),
             labels=labels, adj=0, ...)
}
