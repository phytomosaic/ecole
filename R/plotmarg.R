#' Scatterplot with marginals
#'
#' Scatterplot with marginal densities or orthogonal fit lines, useful
#'     for visualizing gradients on ordinations.
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
#' @param marg
#' either \code{'dens'} for density plots,
#'     or \code{'ortho'} for orthogonal fitlines
#'
#' @return
#' A ggplot2 object
#'
#' @examples
#' require(vegan)
#' data(dune, dune.env)
#' id <- cbind(scores(metaMDS(dune)), dune.env)
#' id$Moisture <- as.numeric(levels(id$Moisture))
#' plotmarg(d=id, x='NMDS1', y='NMDS2', over='Moisture', marg='dens')
#' plotmarg(d=id, x='NMDS1', y='NMDS2', over='Moisture', marg='ortho')
#'
#' @export
`plotmarg` <- function(d, x, y, over, marg=c('dens','ortho'), ... ){

     require(ggplot2)         # for plotting
     require(gridExtra)       # for plotting
     require(grid)            # for plotting
     require(gtable)          # for arranging plots

     xx   <- as.numeric((d[, grep(paste0('^', x,'$'), names(d))]))
     yy   <- as.numeric((d[, grep(paste0('^', y,'$'), names(d))]))
     pp   <- as.numeric((d[, grep(paste0('^', over,'$'), names(d))]))
     marg <- match.arg(marg)
     oldtheme  <- theme_get()
     on.exit(theme_set(oldtheme))
     theme_set(theme_classic() + theme(legend.position='none'))
     theme_rm  <- theme(axis.text         = element_blank(),
                        axis.ticks        = element_blank())
     # main scatterplot
     p1 <- ggplot(d, aes_string(x, y)) +
          geom_point(aes_string(color=over), alpha=0.7) +
          scale_x_continuous(expand = c(0, 0)) +
          scale_y_continuous(expand = c(0, 0)) +
          expand_limits(y = c(min(yy) - .1*diff(range(yy)),
                              max(yy) + .1*diff(range(yy))))  +
          expand_limits(x = c(min(xx) - .1*diff(range(xx)),
                              max(xx) + .1*diff(range(xx))))  +
          theme(plot.margin= unit(c(0, 0, 0.5, 0.5), 'lines'))

     if(marg=='ortho'){
          # horiz plot
          taux <- round(cor(xx, pp, 'na.or.complete', 'kendall'), 2)
          taux <- paste0('Kendalls tau = ', taux)
          pca <- prcomp(~xx+pp)
          slp <- with(pca, rotation[2,1] / rotation[1,1])
          int <- with(pca, center[2] - slp*center[1])
          p2  <- ggplot(d, aes_string(x=x, y=over)) +
               geom_point(color='black',alpha=0.6,shape=1,size=1.5) +
               geom_abline(slope=slp,intercept=int,color='blue') +
               geom_smooth(col='green', method='loess', se=F) +
               scale_x_continuous(expand = c(0, 0)) +
               expand_limits(x = c(min(xx) - .1*diff(range(xx)),
                                   max(xx) + .1*diff(range(xx)))) +
               theme_rm +
               theme(plot.margin= unit(c(0.5, 0, 0, 0.5), 'lines'),
                     axis.title.x = element_blank()) +
               geom_text(label=taux, size=rel(3),x=Inf,y=-Inf,
                         hjust=1.2,vjust=-1.2)
          # vertical plot
          tauy <- round(cor(yy, pp, 'na.or.complete', 'kendall'), 2)
          tauy <- paste0('Kendalls tau = ', tauy)
          pca <- prcomp(~yy+pp)
          slp <- with(pca, rotation[2,1] / rotation[1,1])
          int <- with(pca, center[2] - slp*center[1])
          p3  <- ggplot(d, aes_string(x=y, y=over))  +
               geom_point(color='black',alpha=0.6,shape=1,size=1.5)+
               geom_abline(slope=slp, intercept=int, color='blue') +
               geom_smooth(col='green', method='loess', se=F) +
               scale_x_continuous(expand = c(0, 0)) +
               expand_limits(x = c(min(yy) - .1*diff(range(yy)),
                                   max(yy) + .1*diff(range(yy))))  +
               coord_flip() + theme_rm +
               theme(plot.margin= unit(c(0, 0.5, 0.5, 0), 'lines'),
                     axis.title.y = element_blank()) +
               geom_text(label=tauy, size=rel(3), x=Inf, y=Inf,
                         hjust=1.2, vjust=1.2)
     }
     if(marg=='dens'){
          # horiz plot
          p2 <- ggplot(d, aes_string(x = x)) +
               stat_density(geom='path', position='identity') +
               scale_x_continuous(expand = c(0, 0)) +
               expand_limits(x = c(min(xx) - .1*diff(range(xx)),
                                   max(xx) + .1*diff(range(xx))))  +
               theme_rm +
               theme(plot.margin= unit(c(0.5, 0, 0, 0.5), 'lines'),
                     axis.title.x = element_blank())
          # vertical plot
          p3 <- ggplot(d, aes_string(x = y)) +
               stat_density(geom='path', position='identity') +
               scale_x_continuous(expand = c(0, 0)) +
               expand_limits(x = c(min(yy) - .1*diff(range(yy)),
                                   max(yy) + .1*diff(range(yy))))  +
               coord_flip() + theme_rm +
               theme(plot.margin= unit(c(0, 0.5, 0.5, 0), 'lines'),
                     axis.title.y = element_blank())
     }

     # get gtables and set width/height
     gt1 <- ggplot_gtable(ggplot_build(p1))
     gt2 <- ggplot_gtable(ggplot_build(p2))
     gt3 <- ggplot_gtable(ggplot_build(p3))
     maxWidth  <- unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
     maxHeight <- unit.pmax(gt1$heights[4:5], gt3$heights[4:5])
     gt1$widths[2:3]  <- as.list(maxWidth)
     gt2$widths[2:3]  <- as.list(maxWidth)
     gt1$heights[4:5] <- as.list(maxHeight)
     gt3$heights[4:5] <- as.list(maxHeight)

     # combine all and render
     gt <- gtable(widths=unit(c(7, 2),'null'),
                  height=unit(c(2, 7),'null'))
     gt <- gtable_add_grob(gt, gt1, 2, 1)
     gt <- gtable_add_grob(gt, gt2, 1, 1)
     gt <- gtable_add_grob(gt, gt3, 2, 2)
     grid.newpage()
     grid.draw(gt)
}