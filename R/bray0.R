#' Zero-adjusted Bray-Curtis dissimilarity
#'
#' Dissimilarity measure when some sites are empty or share no species
#'     in common with other sites.
#'
#' @param x
#' array of species data, where rows = SUs and cols = species
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' A distance object of class 'dist'.
#'
#' @details
#' In community data with high beta-diversity, you may find that many
#'     pairs of sample units do not share any species in common.  In
#'     this situation, bounded dissimilarity measures such as
#'     Bray-Curtis tend to 'saturate' at 1, and which provide little
#'     information about 'true' dissimilarity of 'no-share' sample
#'     unit pairs (Smith 2017).  Clarke et al. (2006) proposed a
#'     'zero-adjusted Bray-Curtis' measure by effectively adding a
#'     dummy species to the every sample unit prior to calculation of
#'     the dissimilarity matrix.  The dummy species takes on the
#'     smallest nonzero value. Clarke et al. (2006) recommend only
#'     doing so when two sample units are empty for the same reason.
#'     Consider carefully whether this approach makes sense for you.
#'
#' @examples
#' ###   create data with several no-share SU pairs
#' set.seed(23)
#' Nspp <- Nsite <- 12
#' (x <- matrix(floor(rbeta((Nspp*Nsite), .2, 10)*100), Nspp, Nsite))
#' ecole::mx_diversity(x)   # high proportion of no-share SU pairs
#' d0 <- vegdist(x, 'bray')
#' d1 <- bray0(x)
#' op <- par(mfrow=c(1,3))
#' ordiplot(cmdscale(d0), type='t')
#' ordiplot(cmdscale(d1), type='t')
#' plot(d0,d1) ; abline(0,1) # zero-adj BC 'fans out' dissimilarities
#' par(op)
#'
#' ###   BC and zero-adj BC are ~same when no-share SUs are absent
#' x <- x+10
#' ecole::mx_diversity(x)   # zero no-share SU pairs
#' plot(vegdist(x, 'bray'), bray0(x)) ; abline(0,1)
#'
#' ###   test data from Table 1 of Clarke et al. (2006)
#' x <- matrix(c(0,0,1,1,0,1,0,0,10,10,0,0,
#'               0,0,0,0,1,0,0,5,0,20,0,50,
#'               0,0,0,0,0,1,0,5,0,20,0,50,
#'               0,0,0,0,0,0,2,0,10,0,50,100), nrow=12, ncol=4)
#' d0 <- vegdist(x, 'bray')
#' d1 <- bray0(x)
#' s  <- 2:12
#' # cf. 'B–C dissimilarity' in Table 1:
#' as.matrix(d0)[cbind(s,s-1)]
#' # cf. 'Zero-adjusted B–C' in Table 1:
#' as.matrix(d1)[cbind(s,s-1)]
#' # NMS ordinations using each dissimilarity matrix
#' #     cf. Fig. 1 in Clarke et al. (2006)
#' d0 <- as.dist(as.matrix(d0)[-c(1:2),-c(1:2)]) # rm depauperate sites
#' m0 <- metaMDS(d0, try=99)
#' m1 <- metaMDS(d1, try=99)
#' scr0 <- scores(m0)*(-1)  # simple reflection
#' scr1 <- scores(m1)*(-1)
#' op <- par(mfrow=c(1,2))
#' ordiplot(scr0, type='t')
#' ordiplot(scr1, type='t')
#' par(op)
#'
#' @references
#' Clarke, K.R., P.J. Somerfield, and M.G. Chapman. 2006. On
#'     resemblance measures for ecological studies, including
#'     taxonomic dissim and a zero-adjusted Bray–Curtis coefficient
#'     for denuded assemblages. J Exp Marine Biol and Ecol 330:55–80.
#'
#' Smith, R.J. 2017. Solutions for loss of information in
#'     high-beta-diversity community data. Methods in Ecology and
#'     Evolution 8(1): 68-74.
#'
#' @seealso
#' vegan's \code{\link[vegan]{stepacross}} is a conceptual
#'     alternative and uses a dissimilarity matrix (not raw data).
#'
#' @export
#' @rdname bray0
`bray0` <- function(x, ...){
     x   <- as.matrix(x)
     val <- min(x[x!=0], ...)           # smallest non-zero value
     x   <- cbind(x, rep(val, nrow(x))) # add pseudo-species
     return(vegan::vegdist(x, method='bray'))
}