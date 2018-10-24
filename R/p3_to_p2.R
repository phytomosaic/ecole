#' Convert P3Veg to P2Veg
#'
#' Thin a species abundance matrix collected via all-inclusive P3Veg
#'      as though it were collected via semi-inclusive P2Veg.
#'
#' @param x
#' array of 'P3Veg' species data, where rows = SUs and cols = species
#'
#' @param keylist
#' list of species (col 1) and their growth habit group (col 2)
#'
#' @param ...
#' further arguments passed to other methods
#'
#' @return
#' Array of 'P2Veg' species data.
#'
#' @details
#' National FIA Phase 3 vegetation (P3Veg) sampling is 'all-inclusive'
#' : it captures all species that occur within or overhanging a plot
#' (based on nested quadrats and subplots in a plot).  By contrast,
#' FIA Phase 2 vegetation (P2Veg) sampling is 'semi-inclusive': it
#' includes only the 4 most abundant species within each growth
#' habit group (tree seedlings and saplings, shrubs/woody vines,
#' forbs, graminoids, and overstory trees) that occupy at least 3%
#' canopy cover on each 7.3-m radius subplot.
#'
#' NB: this function only applies to FIA 'subplots', not plot-level.
#'
#' @examples
#' # fake data
#' set.seed(23)
#' Nspp <- Nsite <- 99
#' x <- matrix(floor(rbeta(Nspp*Nsite, .2, 10)*100), Nspp, Nsite)
#' dimnames(x)[[2]] <- paste0('Sp_', 1:Nspp)
#'
#' # fake keylist
#' v <- c('seed/sap','shrub','forb','graminoid','tree')
#' s <- dimnames(x)[[2]]
#' keylist <- data.frame(species=s,
#'                       growthhabit=v[round(runif(length(s),1,5),0)])
#'
#' # convert P3 to P2 Veg
#' res <- p3_to_p2(x, keylist)
#'
#' # check, should be 20 (or fewer) per row
#' res[res>0] <- 1
#' any(rowSums(res) != 20) # expect FALSE
#' apply(  x > 0, 1, sum) # old P3 species count per site
#' apply(res > 0, 1, sum) # new P2 species count per site, expect <=20
#'
#' @references
#'  FIA [Forest Inventory and Analysis Program]. 2012. Supplement to
#'     the Alaska Field Guide for Installation of FIA Plots in
#'     Experimental Forests and Associated Research Areas. 107 pgs.
#'     USDA Forest Service, Pacific Northwest Research Station,
#'     Anchorage, Alaska.
#'
#'  FIA [Forest Inventory and Analysis Program]. 2016. Field
#'     Instructions for the Annual Inventory of Coastal Alaska 2016.
#'     420 pgs. USDA Forest Service, Pacific Northwest Research
#'     Station, Anchorage, Alaska.
#'
#' @export
#' @rdname p3_to_p2
`p3_to_p2` <- function(x, keylist, ...){

     # check species data structure
     wasDataFrame <- is.data.frame(x)
     x   <- as.matrix(x)
     rng <- range(x, na.rm=TRUE)
     if(any( rng<0 | rng>100 )) stop('data not in 0-100 range')

     # cover must be at least 3%
     x[x<3] <- 0L

     # match growth habit for each species
     spnm <- dimnames(x)[[2]]
     gh   <- keylist[match(spnm, keylist[,1]),2]

     # for each row, keep top 4 per growth habit
     for(i in 1:dim(x)[[1]]){
          tmp <- data.frame(val=x[i,], gh)             # pick one row
          ord <- tmp[order(tmp$val,decreasing=TRUE),]  # order it
          top <- Reduce(rbind,by(ord,ord['gh'],head,n=4))# top4 by grp
          keep<- dimnames(top)[[1]]
          x[i, !names(x[i,])%in%keep] <- 0L # zero out if not in top4
     }

     # cleanup and return
     if (wasDataFrame){x <- as.data.frame(x)}
     return(x)
}
