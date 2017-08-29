#' Diversity of a species matrix
#'
#' Calculate diversity properties of a species matrix:
#'     Gamma, Alpha, Beta, Halfchanges, Dust Bunny Index, others.
#'
#' @param x
#' array of species data, where rows = SUs and cols = species
#'
#' @return
#' Dataframe with 8 numeric values: \itemize{
#'   \item gamma = count of species in entire matrix
#'   \item alpha = average count of species per SU
#'   \item beta  = Whittaker's beta diversity = [(gamma/alpha) - 1]
#'   \item halfchanges = beta diversity by exponential transformation
#'             of average Sorenson dissimilarities (pg 31 of McCune &
#'             Grace 2002)
#'   \item dbi   = Dust Bunny Index of McCune and Root (2015)
#'   \item prop0 = proportion of zero elements (McCune and Root 2015)
#'   \item propnoshare = proportion of no-share SU pairs
#'   \item N     = number of SUs
#'   }
#'
#' @details
#' Calculates eight measures of diversity in the matrix. For an
#'     example applied to many simulated datasets, see Smith (2017).
#'
#' @examples
#' # species abundance data
#' set.seed(1917)
#' spe <- data.frame(matrix(rnorm(30, 10, 50), 10, 3))
#' spe[spe<0] <- 0
#' colnames(spe) <- c('Acer rubrum','Acer saccharum','Acer saccharinum')
#' spe
#' mx_diversity(spe)
#'
#' @references
#' McCune, B., and J. B. Grace. 2002. Analysis of Ecological
#'     Communities. MjM Software, Gleneden Beach, Oregon, USA. 304 pp.
#'
#' McCune, B., and H.T. Root. 2015. Origin of the dust bunny
#'     distribution in ecological community data. Plant Ecology
#'     216(5): 645-656.
#'
#' Smith, R.J. 2017. Solutions for loss of information in
#'     high-beta-diversity community data. Methods in Ecology and
#'     Evolution 8(1): 68-74.
#'
#' @export
`mx_diversity` <- function(x, ...){

     require(vegan)

     # function to calculate propn of noshare sites
     `noshare` <- function(x){
          z <- vegan::no.shared(x)
          length(z[z==TRUE]) / length(z)
     }

     # Dust Bunny Index, McCune and Root (2015)
     `dbi` <- function(x, method, ...){
          x <- as.matrix(x)
          method <- match.arg(method, c('propzero', 'dbi'))
          if(method == 'dbi'){
               DBI <- 1 - mean(as.matrix(decostand(x, method='max')))
               out <- DBI
          }else{
               dims  <- nrow(x)*ncol(x)
               prop0 <- sum(x==0) / dims
               # maxprop0 <- 1 - 1/dims # finds MAX possible prop0
               out <- prop0
          }
          out <- round(out, 3)
          out
     }

     # halfchanges (pg 31 McCune & Grace 2002)
     `hc` <- function(x, method='bray', na.rm=T, ...){
          require(vegan)
          D     <- vegan::vegdist(x=x, method=method, na.rm=na.rm)
          Dbar  <- mean(D, na.rm=na.rm)
          betaD <- log( 1 - Dbar ) / log(0.5)
          betaD <- round(betaD, 2)
          betaD
     }

     out <- data.frame(
          gamma = length(which(colSums(x) > 0)),
          alpha = mean(apply(x > 0, MARGIN=1, sum)),
          beta  = length(which(colSums(x) > 0)) /
               mean(apply(x > 0, MARGIN=1, sum)) - 1,
          halfchanges = hc(x),
          dbi = dbi(x, 'dbi'),
          prop0 = dbi(x, 'propzero'),
          propnoshare = noshare(x),
          N = nrow(x)
     )
     out <- apply(out, 1, round, 2)
     colnames(out) <- 'mx_div'
     out
}