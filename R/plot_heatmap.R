#' @title Heatmap of species abundance matrix
#'
#' @description
#' Plot site x species matrix with heatmap colors and proper labels.
#'
#' @param x array of species data, where rows = SUs and cols = species
#'
#' @param xord,yord logical, should rows, columns be ordered by mean?
#'
#' @param logbase numeric, logarithm base to increase plot contrast
#'
#' @param labcex numeric, label expansion factor; defaults to 0.7
#'
#' @param col  vector defining a color palette
#'
#' @param xexp,yexp   expansion factor making room for x,y labels
#'
#' @param ...    further arguments passed to other methods
#'
#' @return
#' None, plots to device.
#'
#' @details
#' Plots a heatmap to quickly visualize species abundances/occurences.
#'
#' @examples
#' data(smoky)
#' z <- smoky$spe
#' plot_heatmap(z, yord=FALSE, yexp=1.7)
#' plot_heatmap(z, yord=FALSE, yexp=1.7, logbase=10)
#' plot_heatmap(z, yord=FALSE, yexp=1.7, logbase=10, asp=1)
#' plot_heatmap(z, xord=FALSE, yord=FALSE, yexp=1.7, logbase=10,
#'              asp=1)
#' r <- colorRampPalette(c("transparent","darkgreen"))(99)
#' plot_heatmap(z, xord=FALSE, yord=FALSE, yexp=1.7, logbase=10,
#'              asp=1, col=r)
#'
#' @export
#' @rdname plot_heatmap
`plot_heatmap` <- function(x, xord=TRUE, yord=FALSE, logbase=FALSE,
                           labcex=0.7, col, xexp=1, yexp=1, ...){
     r <- c('transparent',
            '#5E4FA2','#4F61AA','#4173B3','#3386BC','#4198B6',
            '#51ABAE','#62BEA6','#77C8A4','#8ED1A4','#A4DAA4',
            '#B8E2A1','#CBEA9D','#DEF199','#EAF69F','#F2FAAC',
            '#FAFDB8','#FEFAB6','#FEF0A5','#FEE695','#FDD985',
            '#FDC978','#FDB96A','#FCA75E','#F99254','#F67D4A',
            '#F26943','#E85A47','#DE4B4B','#D33C4E','#C1284A',
            '#AF1446','#9E0142')
     if(missing(col)) col <- r
     if(xord) x <- x[,order(-colMeans(x))]
     if(yord) x <- x[ order(rowMeans(x)),]
     if(is.numeric(logbase)){
          x <- log(x+min(x[x!=0], na.rm=TRUE), base=logbase)
     }
     op <- par(mfrow=c(1,1), mar=c(0,2.5*xexp,5*yexp,0)+.3,
               oma=c(0,0,0,0),font=2)
     image(1:ncol(x), 1:nrow(x), t(x), col=col, axes=F, ...)
     axis(3, at=1:ncol(x), labels=colnames(x), las=3, tick=F,
          cex.axis=labcex)
     axis(2, at=1:nrow(x), labels=rownames(x), las=1,
          cex.axis=labcex)
     par(op)
}
