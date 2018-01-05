#' Multivariate outliers
#'
#' Find extreme values in ecological community data
#'
#' @param x  array of species data
#'
#' @param mult
#' SD threshold for 'outliers' = the number of SD's away from
#'    the mean \emph{distance} at which an outlier is defined;
#'    default is 3 times the SD
#'
#' @param method
#' distance measure; follows \code{\link[vegan]{vegdist}}
#'
#' @param metric
#' defaults to 'SD'; only other alternative is 'MAD'
#'
#' @param plot
#' logical, should a histogram of distances be drawn?
#'
#' @param perc
#' numeric, returns outlier values beyond the specified percentile
#'
#' @param pick
#' numeric, returns this number of most extreme outliers
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' A data frame with plot identifiers, calculated distances, and
#'     standard deviations from the mean distance, either with
#'     or without a histogram.
#'
#' @details
#' Outlier analysis based on community distances. Emulates PC-ORD, but
#'     see also McCune and Grace (2002).
#'
#' @examples
#' require(vegan)
#' data(dune)
#' set.seed(19)
#' spe <- rbind(dune, runif(ncol(dune),0,1000)) # add an outlier row
#' outlier_multi(spe, mult=3, plot=FALSE)
#' outlier_multi(spe, mult=3)
#' outlier_multi(spe, perc=5)
#' outlier_multi(spe, pick=5)
#'
#' @family outlier functions
#' @seealso \code{\link{outlier_uni}} for univariate analog.
#'
#' @references
#' McCune, B., and J. B. Grace. 2002. Analysis of Ecological
#'     Communities. MjM Software, Gleneden Beach, Oregon, USA. 304 pp.
#'
#' @export
`outlier_multi` <- function(x, mult=3, method='bray', metric='SD',
                            plot=TRUE, perc=NULL, pick=NULL, ... ){
  d  <- as.matrix(
    vegan::vegdist(x, method=method, binary=F, diag=T, upper=T)
  )
  diag(d)  <- as.numeric(1)
  ad       <- apply(d, 2, mean) # avg dist for each entity
  if(metric=='SD'){
    grandm <- mean(ad)
    v      <- sd(ad)
    trip   <- v*mult
    crit   <- grandm + trip
  }
  else if(metric=='MAD'){
    grandm <- median(ad)
    v      <- mad(ad)
    trip   <- v*mult
    crit   <- grandm + trip
  }else{
    stop('Threshold metric not valid')
  }
  ad <- data.frame(pid=names(ad), dist=ad, StdDevs=(ad-grandm)/(v))
  if(all(ad$dist < crit))
    cat('No outliers detected by this criterion')
  if(!is.null(perc)){
    plot  <- FALSE
    ad    <- ad[order(ad$dist, decreasing=T), ]
    nkeep <- round( nrow(ad)*perc/100 )
    out   <- ad[c(1:nkeep ),]
    cat('Outliers: these distances are in ', 100-perc,
        'th percentile of values:\n', sep='')
    return(out)
  }
  if(!is.null(pick)){
    plot  <- FALSE
    ad    <- ad[order(ad$dist, decreasing=T), ]
    nkeep <- pick
    out   <- ad[c(1:nkeep ),]
    cat('Outliers: these distances are top ', pick,
        ' values:\n', sep='')
    return(out)
  }
  else{
    if(plot){
      hist(ad$dist, breaks=19, col='grey', main=NULL,
           xlab='Distances')
      abline(v=crit, col='red')
    }
    cat('Outliers: these distances exceed', round(trip,4),
        'from the center', round(grandm,4), ':\n', sep=' ')
    out <- ad[which(ad$dist >= crit),]
    out
  }
}
