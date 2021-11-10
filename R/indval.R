#' @title (Blocked) Indicator Species Analysis
#'
#' @description
#' Blocked or unblocked versions of Dufrene-Legendre indicator species analysis.
#'    If blocked, then can also calculate conditional probabilities of
#'    gain/loss/stasis using Bayes rule. If blocked, then blocks are assumed to
#'    be sample units and groups are assumed to be the two sample events.
#'
#' @param spe array, where rows = SUs and cols = species.  Values may be numeric
#'    abundances or presence/absence.
#'
#' @param grp vector, describing group membership (required).
#'
#' @param blk vector, describing block membership (optional).
#'
#' @param cond_probs logical, should conditional probabilities be calculated for
#'    when blocked structure exists? Default `TRUE`.
#'
#' @param ... further arguments currently ignored.
#'
#' @return
#' List containing items:\cr
#'     - \code{cond_probs}:  conditional probabilities, when blocks exist.\cr
#'     - \code{A}: relative abundance component of IndVal per group.\cr
#'     - \code{B}: relative frequency component of IndVal per group.\cr
#'     - \code{IV}: IndVal, the indicator value per group.\cr
#'     - \code{IVmax}: maximum IndVal in the indicated group.\cr
#'     - \code{IVmax_dir}: signed max IndVal, when blocks exist.\cr
#'     - \code{maxgrp}: the group in which each species attains max IndVal.\cr
#'     - \code{sumIVmax}: sum of max IndVal across all species.\cr
#'     - \code{sig_expected}: number of significant indicator species expected
#'                                                                at random.\cr
#'     - \code{pval}: permutation p-values for each species.\cr
#'
#' @details
#' Unblocked version follows Dufrene and Legendre (1997).  Blocked indicator
#'    species analysis was first available in PC-ORD (McCune and Mefford 2017).
#'    For blocked designs, the current function can also calculate conditional
#'    probabilities of species gains, losses, and stasis across two sampling
#'    events.  Conditional probabilities use Bayes' rule.
#'
#' @references
#' Dufrene, M. and Legendre, P. 1997. Species assemblages and indicator
#'     species: the need for a flexible asymmetrical approach. Ecological
#'     Monographs 67(3):345-366.
#'
#' McCune, B., and M. J. Mefford. 2017. PC-ORD. Multivariate Analysis
#'     of Ecological Data. Version 7. MjM Software Design, Gleneden
#'     Beach, OR.
#'
#' @examples
#' data(braun)
#' spe <- braun$spe[-1,]                         # force even number
#'
#' # no group structure exists, so lets pretend we have revisit data
#' blk <- factor(rep(1:(NROW(spe)/2), times=2))  # 173 sites 'visited' twice
#' grp <- factor(rep(c(1,2), ea=NROW(spe)/2))    # 'visits' 1 or 2 for each site
#'
#' # unblocked
#' iv  <- indval(spe, grp)
#'
#' # blocked by site
#' ivb <- indval(spe, grp, blk, cond_probs=TRUE)
#'
#' # see effects of blocking
#' plot(iv$IVmax, ivb$IVmax) ; abline(0,1)
#' table(unblocked = iv$maxgrp, blocked = ivb$maxgrp)
#'
#' # given our haphazard group assignment, most species have poor indicator value
#' hist(ivb$IVmax, breaks=22, xlab='Max IndVal', main='')
#' iv$sig_expected   # num indicator species expected at random
#'
#' # see how IndVal compares to conditional gain/loss probabilities blocked by site
#' a <- data.frame(ivb$cond_probs)
#' plot(a$prloss, a$prgain, col=ecole::colvec(ivb$IVmax_dir), pch=16)
#'
#' @seealso \code{\link[labdsv]{indval}} or \code{\link[indicspecies]{strassoc}}
#'
#' @export
#' @rdname indval
`indval` <- function(spe, grp, blk = NULL, cond_probs = TRUE, ...) {
    # *blocked and unblocked* Dufrene and Legendre's IndVal,
    #    along with *Bayes conditional probabilities* of gain/loss/stasis
    #      Rob Smith, phytomosaic@gmail.com, 05 Nov 2021
    #       GNU General Public License, Version 3.0
    IVzmax <- pr <- NA # setup
    # compute conditional transition probabilities, using Bayes rule
    if (!is.null(blk) & cond_probs) {
        # function for Bayes rule
        `bayes` <- function(v1, v2) {
            # table of joint and marginal probabilities
            tab <- table(v1,v2)
            # correction if either visit had ALL presences or ALL absences
            dn <- dimnames(tab)
            if (all(dn[[1]] == '0')) tab <- rbind(tab, '1'=c(0,0))
            if (all(dn[[1]] == '1')) tab <- rbind('0'=c(0,0), tab)
            if (all(dn[[2]] == '0')) tab <- cbind(tab, '1'=c(0,0))
            if (all(dn[[2]] == '1')) tab <- cbind('0'=c(0,0), tab)
            # proceed
            tx  <- addmargins(tab) # joint and marginal *counts*
            tot <- tx[3,3]         # marginal grand total
            p   <- tx / tot        # joint and marginal *probabilities*
            # GAIN
            pB_A <- p[1,2]         # pr absence at visit-1, given presence at visit-2
            pB   <- p[1,3]         # pr absence at visit-1
            pA   <- p[3,2]         # pr presence at visit-2, from any initial state
            pA_B <- pB_A * pA / pB # cond pr presence at visit-2, from initial absence
            prgain <- pA_B         # conditional probability of GAIN
            if(any(c(pB_A,pA,pB) == 0)) prgain <- 0 # handle zero-division
            # LOSS
            pB_A <- p[2,1]         # pr presence at visit-1, given absence at visit-2
            pB   <- p[2,3]         # pr presence at visit-1
            pA   <- p[3,1]         # pr absence at visit-2, from any initial state
            pA_B <- pB_A * pA / pB # cond pr absence at visit-2, from initial presence
            prloss <- pA_B         # conditional probability of LOSS
            if(any(c(pB_A,pA,pB) == 0)) prloss <- 0 # handle zero-division
            # STASIS1
            pB_A <- p[2,2]         # pr presence at visit-1, given presence at visit-2
            pB   <- p[2,3]         # pr presence at visit-1
            pA   <- p[3,2]         # pr presence at visit-2, from any initial state
            pA_B <- pB_A * pA / pB # cond pr presence at visit-2, from initial presence
            prsta1 <- pA_B         # conditional probability of STASIS1
            if(any(c(pB_A,pA,pB) == 0)) prsta1 <- 0 # handle zero-division
            # STASIS0
            pB_A <- p[1,1]         # pr absence at visit-1, given absence at visit-2
            pB   <- p[1,3]         # pr absence at visit-1
            pA   <- p[3,1]         # pr absence at visit-2, from any initial state
            pA_B <- pB_A * pA / pB # cond pr absence at visit-2, from initial absence
            prsta0 <- pA_B         # conditional probability of STASIS0
            if(any(c(pB_A,pA,pB) == 0)) prsta0 <- 0 # handle zero-division
            return(c(prgain=prgain, prloss=prloss, prsta0=prsta0, prsta1=prsta1))
        }
        # TODO: checks to ensure 1 -> 2 order -- right now this is only assumed!
        # TODO: check that this can handle *unbalanced* group sizes...
        ii <- grp == levels(grp)[1] # index *initial* visits
        ff <- grp == levels(grp)[2] # index *final* visits
        pa <- (spe > 0) * 1         # force binary presence/absence
        pr <- t(sapply(1:NCOL(pa), function(j) bayes(v1 = pa[ii,j], v2 = pa[ff,j])))
        rownames(pr) <- colnames(pa)
    }
    # # TO DELETE: conditional transition probabilities, accounting for blocks
    # if (!is.null(blk) & cond_probs) {
    #   pa <- (spe > 0) * 1  # force binary presence/absence
    #   pr <- matrix(NA,     # initialize cond probs
    #                nrow = NCOL(spe), ncol=4,
    #                dimnames=list(colnames(spe),
    #                              c('gain','stasis0','stasis1','loss')))
    #   ct <- matrix(NA,     # initialize transition counts
    #                nrow = length(unique(blk)),
    #                ncol = NCOL(spe),
    #                dimnames=list(unique(blk), colnames(spe)))
    #   for (j in 1:NCOL(spe)) {
    #     ii  <- 1:length(unique(blk)) # row index for *initial* visits
    #     p0  <- sum(pa[ii,j] == 0)    # p(x|0) = marginal prob of initial abs
    #     p1  <- sum(pa[ii,j] == 1)    # p(x|1) = marginal prob of initial pres
    #     for (b in unique(blk)) {
    #       i     <- blk == b      # row index for blocks
    #       stopifnot(sum(i) == 2) # guarantee only 2 obsvns per block
    #       z     <- pa[i, j]      # presence/absence pair
    #       k <- which(c(          # which conditional transition?
    #         z[2] == 1 & z[1] == 0, # ( 1 | 0 ) = gain
    #         z[2] == 0 & z[1] == 0, # ( 0 | 0 ) = stasis0
    #         z[2] == 1 & z[1] == 1, # ( 1 | 1 ) = stasis1
    #         z[2] == 0 & z[1] == 1  # ( 0 | 1 ) = loss
    #       ))
    #       ct[b,j] <- k
    #     } # end block loop
    #     pr[j,1] <- sum(ct[,j] == 1) # ( 1 | 0 ) = gain
    #     pr[j,2] <- sum(ct[,j] == 2) # ( 0 | 0 ) = stasis0
    #     pr[j,3] <- sum(ct[,j] == 3) # ( 1 | 1 ) = stasis1
    #     pr[j,4] <- sum(ct[,j] == 4) # ( 0 | 1 ) = loss
    #     pr[j,1] <- pr[j,1] / p0
    #     pr[j,2] <- pr[j,2] / p0
    #     pr[j,3] <- pr[j,3] / p1
    #     pr[j,4] <- pr[j,4] / p1
    #     pr[is.nan(pr)] <- 0L    # correct any zero-division
    #   } # end species loop
    # } # end conditional probs by block
    if (!is.null(blk)) { # if blocked, then relativize *within* blocks
        for (j in 1:NCOL(spe)) {  # begin BLOCKED IndVal
            for (b in unique(blk)) {
                i     <- blk == b     # row index for blocks
                z     <- spe[i, j]    # raw abundances
                zsum  <- sum(z)       # sum abundances
                spe[i, j] <- if (zsum == 0) z else z / zsum  # relativize
            }
        }
    }
    # proceed with IndVal
    A_num  <- apply(spe, 2, function(j) tapply(j, grp, mean)) # mean abu per grp
    A_den  <- apply(A_num, 2, sum)          # sum of the mean abu across all grps
    A      <- sweep(A_num, 2, A_den, '/')   # A = rel abu per grp = specificity
    B_num  <- apply(spe>0, 2, function(j) tapply(j, grp, sum))  # occ per grp
    B_den  <- c(table(grp))                 # total count of SUs per grp
    B      <- sweep(B_num, 1, B_den, '/')   # B = rel frq per grp = fidelity
    IV     <- A * B                         # IV = IndVal
    IVmax  <- c(pmax(t(IV)[,1], t(IV)[,2])) # max IndVal
    maxgrp <- apply(IV, 2, which.max)       # max group
    sumIVmax <- sum(IVmax)                  # sum of max IndVal across spp
    alpha    <- c(0.050, 0.010, 0.001)      # significance level
    s        <- NCOL(spe) * alpha           # num signif expected at random
    s        <- data.frame(alpha=as.character(alpha), species_expected = s)
    rownames(s) <- NULL
    IVmax_dir <- IVmax # signed IndVal: POS 'increasers', NEG 'decreasers'
    IVmax_dir[maxgrp == 1] <- IVmax_dir[maxgrp == 1] * (-1) # signed IndVal
    # TODO: permutations....
    return(list(cond_probs = pr,
                A     = t(A),
                B     = t(B),
                IV    = t(IV),
                IVmax           = IVmax,
                IVmax_dir       = IVmax_dir,
                maxgrp          = maxgrp,
                sumIVmax        = sumIVmax,
                sig_expected    = s,
                pval            = NA))
}

# ### TO DELETE: Dufrene and Legendre's IndVal, *blocked and unblocked*
# `indval` <- function(spe, grp, blk = NULL, ...) {
#         ### TODO: check that this can handle *unbalanced* group sizes...
#         if (!is.null(blk)) { # if blocked, then relativize *within* blocks
#                 for (j in 1:NCOL(spe)) {
#                         for (b in unique(blk)) {
#                                 i     <- blk == b     # row index for blocks
#                                 z     <- spe[i, j]    # raw abundances
#                                 zsum  <- sum(z)       # sum abundances
#                                 spe[i, j] <- if (zsum == 0) z else z / zsum  # relativize
#                         }
#                 }
#         }
#         # proceed as usual
#         A_num <- apply(spe, 2, function(j) tapply(j, grp, mean)) # mean abu per grp
#         A_den <- apply(A_num, 2, sum)        # sum of the mean abu across all grps
#         A     <- sweep(A_num, 2, A_den, '/') # A = rel abu per grp = specificity
#         B_num <- apply(spe>0, 2, function(j) tapply(j, grp, sum)) # occ per grp
#         B_den <- c(table(grp))                # total count of SUs per grp
#         B     <- sweep(B_num, 1, B_den, '/') # B = rel frq per grp = fidelity
#         IV    <- A * B                       # IV = IndVal
#         IVmax    <- c(pmax(t(IV)[,1], t(IV)[,2])) # max IndVal
#         maxgrp   <- apply(IV, 2, which.max)       # max group
#         sumIVmax <- sum(IVmax)                    # sum of max IndVal across spp
#         # number of significant indicator species expected at random:
#         alpha    <- c(0.050, 0.010, 0.001)
#         s        <- NCOL(spe) * alpha
#         s        <- data.frame(alpha=as.character(alpha), species_expected = s)
#         rownames(s) <- NULL
#         # TODO: permutations....
#         # perm <- 999
#         # res <- replicate(perms, expr = { #  permute sites
#         #   tmp   <- spe[sample.int(NROW(spe), replace=FALSE),]
#         #   max(rel_frq(tmp, grp) * rel_abu(tmp, grp))
#         # })
#         # pval <- (1 + (res >= max(IV))) / (1 + perm)
#         return(list(A=t(A), B=t(B), IV=t(IV), IVmax=IVmax,
#                     maxgrp=maxgrp, sumIVmax=sumIVmax, sig_expected=s,
#                     pval=NA))
# }

####    END    ####
