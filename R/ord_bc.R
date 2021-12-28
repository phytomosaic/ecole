#' @title Bray-Curtis ordination, aka polar ordination
#'
#' @description Bray-Curtis ordination.
#'
#' @param x array, where rows = SUs and cols = variables such as environmental
#'   or traits values.
#'
#' @param method dissimilarity index, passed to `vegan::vegdist`.
#'
#' @param endmethod Endpoint selection method. `'var_reg'` is the variance
#'     regression method now default in PC-ORD (McCune and Mefford 2017).
#'     `'BC_orig'` is Bray-Curtis original method of selecting most-distant SUs.
#'     `'PCORD_orig'` is the deprecated method from PC-ORD, whereby the first
#'     endpoint has the greatest sum of distances from other sites, and the
#'     second endpoint has the greatest distance from the first endpoint.
#'
#' @param ends Numeric, index two sample units as endpoints on first axis
#'     (optional). Endpoints in higher dimensions then selected by `endmethod`.
#'
#' @param ... further arguments not currently passed to other functions.
#'
#' @return List containing items:\cr
#' - \code{sumtab}:  summary table containing for each axis: sums-of-squares of
#'       residual distances, percent variance explained, and sample units
#'       selected as endpoints.\cr
#' - \code{scores}:  ordination scores.\cr
#'
#' @details Bray-Curtis ordination. See references.
#'
#' @references
#'
#' Beals, E.W. 1984. Bray-Curtis ordination: an effective strategy for analysis
#' of multivariate ecological data. Advances in Ecological Research 14: 1-55.\cr
#'
#' Bray, J.R., and J.T. Curtis. 1957. An ordination of the upland forest
#' communities of southern Wisconsin. Ecological Monographs 27: 325-349.\cr
#'
#' McCune, B. and E.W. Beals. 1993. History of the development of Bray-Curtis
#' ordination. Pgs 67-80 in J.S. Fralish, R.P. McIntosh, and O.L. Loucks (eds).
#' John T. Curtis: Fifty Years of Wisconsin Plant Ecology, Wisconsin Academy of
#' Science, Arts, and Letters, Madison, WI.\cr
#'
#' McCune, B., and M.J. Mefford. 2017. PC-ORD. Multivariate Analysis of
#' Ecological Data. Version 7. MjM Software Design, Gleneden Beach, OR.\cr
#'
#' @examples
#' data(smoky, package='ecole')
#' spe <- smoky$spe
#' o1 <- ord_bc(spe, method='bray', endmethod='BC_orig')
#' o2 <- ord_bc(spe, method='bray', endmethod='BC_orig', ends=c(1,12))
#' ecole::set_par(2)
#' plot(scores(o1), type='n')
#' text(scores(o1))
#' plot(scores(o2), type='n')
#' text(scores(o2))
#'
#' @export
#' @rdname ord_bc
`ord_bc` <- function(x,
                     method = 'bray',
                     endmethod = c('var_reg', 'BC_orig', 'PCORD_orig'),
                     ends = c(NA,NA),
                     ...) {
    # Original author: Ken Aho, https://sites.google.com/a/isu.edu/aho/
    # tarball:  https://drive.google.com/file/d/0B65xY-gnYYGramVGcURnZGIyZGM/
    # Modified: Rob Smith, modified from `plant.ecol::polar.ord()`
    z <- c('var_reg', 'BC_orig', 'PCORD_orig')
    endmethod  <- z[pmatch(endmethod, z)][1]
    if (inherits(x, 'dist')) {
        D  <- x
    } else {
        D  <- vegan::vegdist(x, method)
    }
    rn    <- attr(D, 'Labels') # row names
    nr    <- attr(D, 'Size')   # num rows
    n     <- length(D)         # num elements
    SStot <- sum(D ^ 2)        # SST
    D     <- as.matrix(as.dist(D, upper = TRUE, diag = TRUE)) # full matrix
    `allpairs` <- function(n = nr) {
        m <- matrix(1:n, ncol=n, nrow=n)
        list(first = t(m)[lower.tri(m)], second = m[lower.tri(m)])
    }
    pw <- allpairs(nr)
    `getDmat` <- function(umat) {
        Dmat <- matrix(0, ncol = nr, nrow = nr)
        for (i in 1:nrow(mat1)) {
            Dmat[umat[, 1][i], ][umat[, 2][i]] <- umat[, 3][i]
            Dmat[umat[, 2][i], ][umat[, 1][i]] <- umat[, 3][i]
        }
        Dmat
    }
    if (endmethod == 'BC_orig') { ### -------------------------------------------
        `getendp` <- function(mat, RD, dim = 1) {
            endp <- mat[mat[,3] == max(RD), ]
            if (!is.null(nrow(endp))) {
                A <- as.numeric(endp[1,1])
                B <- as.numeric(endp[1,2])
            }
            if (is.null(nrow(endp))) {
                A <- as.numeric(endp[1])
                B <- as.numeric(endp[2])
            }
            s     <- seq(1:nr)
            C     <- s[!s %in% c(A,B)]
            return(list(A=A, B=B, C=C))
        }
    }
    else if (endmethod == 'PCORD_orig') { ### -------------------------------------------
        `getendp` <- function(mat, RD, dim = 1) {
            if (dim == 1) {
                endD <- getDmat(mat)                  ## <<------ duplicates the dissimilarity matrix  ! ! ! ! !
                A <- which.max(apply(endD, 2, sum))
                B <- which.max(endD[,A])
            }
            if (dim > 1) {
                endp <- mat[mat[,3] == max(RD), ]
                if (!is.null(nrow(endp))) {
                    A <- as.numeric(endp[1,1])
                    B <- as.numeric(endp[1,2])
                }
                if (is.null(nrow(endp))) {
                    A <- as.numeric(endp[1])
                    B <- as.numeric(endp[2])
                }
            }
            s     <- seq(1:nr)
            C     <- s[!s %in% c(A,B)]
            return(list(A=A, B=B, C=C))
        }
    }
    else if (endmethod == 'var_reg') { ### -------------------------------------------
        `getendp` <- function(mat, RD, dim = 1) {
            endD <- getDmat(mat)                 ## <<------ duplicates the dissimilarity matrix  ! ! ! ! !
            s    <- seq(1:nrow(endD))
            diag(endD) <- NA
            `sumsq` <- function(x) {
                sum((x[!is.na(x)] - mean(x, na.rm=TRUE))^2)
            }
            A    <- which.max(apply(endD, 2, function(x) sumsq(x)))
            notA <- s[s != A]
            diag(endD) <- 0
            Bmat <- sapply(1:(nrow(endD)-1), function(i) {
                coef(lm(endD[, notA[i]] ~ endD[,A]))[2]
            })
            B     <- notA[which.min(Bmat)]
            C     <- s[!s %in% c(A,B)]
            return(list(A=A, B=B, C=C))
        }
    }
    `getscores` <- function(pw, dAB, A, B, C, dim = 1) {
        scr <- matrix(ncol = 1, nrow = nr)
        for (i in 2:(nr - 1)) {
            scr[1] <- 0
            scr[nr] <- dAB
            if      (C[i-1] > A & C[i-1] > B) {
                scr[i] <-
                    (dAB^2 +
                         (pw[,3][pw[,1] == A & pw[,2] == C[i-1]]^2) -
                         (pw[,3][pw[,1] == B & pw[,2] == C[i-1]]^2)) / (2*dAB)
            }
            else if (C[i-1] < A & C[i-1] > B) {
                scr[i] <-
                    (dAB^2 +
                         (pw[,3][pw[,2] == A & pw[,1] == C[i-1]])^2 -
                         (pw[,3][pw[,1] == B & pw[,2] == C[i-1]]^2)) / (2*dAB)
            }
            else if (C[i-1] < A & C[i-1] < B) {
                scr[i] <-
                    (dAB^2 +
                         (pw[,3][pw[,2] == A & pw[,1] == C[i-1]])^2 -
                         (pw[,3][pw[,2] == B & pw[,1] == C[i-1]]^2)) / (2*dAB)
            }
            else if (C[i-1] > A & C[i-1] < B) {
                scr[i] <-
                    (dAB^2 +
                         (pw[,3][pw[,1] == A & pw[,2] == C[i-1]])^2 -
                         (pw[,3][pw[,2] == B & pw[,1] == C[i-1]]^2)) / (2*dAB)
            }
        }
        sites <- c(A, C, B)
        scr   <- as.matrix(scr[order(sites), ])
        colnames(scr) <- paste0('Dim', dim)
        scr
    }

    ### Dim 1
    mat1 <- cbind(pw$first, pw$second, D[cbind(pw$second, pw$first)])
    # allow Axis 1 to have subjective endpoints
    if(all(!is.na(ends)) & length(ends) == 2 & is.numeric(ends)) {
        A    <- ends[1]
        B    <- ends[2]
        s    <- 1:nr
        C    <- s[!(s %in% ends)]
    } else {
        ep1  <- getendp(mat1, RD = D, dim = 1)
        A    <- ep1$A
        B    <- ep1$B
        C    <- ep1$C
    }
    # resume
    if (A < B) {
        dAB <- mat1[, 3][mat1[, 1] == A & mat1[, 2] == B]
    }
    if (A > B) {
        dAB <- mat1[, 3][mat1[, 2] == A & mat1[, 1] == B]
    }
    scr1    <- getscores(mat1, dAB, A, B, C, dim = 1)
    end1    <- c(rn[A], rn[B])
    mat1

    ### Dim 2
    Dist2   <- sapply(1:n, function(i) (scr1[pw$first[i]] - scr1[pw$second[i]]) ^ 2)
    Dist2_1 <- cbind(pw$first, pw$second,
                     sqrt(ifelse(mat1[,3]^2-Dist2 < 0, 0, mat1[,3]^2-Dist2)))
    ep2     <- getendp(Dist2_1, RD = Dist2_1[, 3], dim = 2)
    A       <- ep2$A
    B       <- ep2$B
    C       <- ep2$C
    if (A < B) {
        dAB <- Dist2_1[, 3][Dist2_1[, 1] == A & Dist2_1[, 2] == B]
    }
    if (A > B) {
        dAB <- Dist2_1[, 3][Dist2_1[, 2] == A & Dist2_1[, 1] == B]
    }
    scr2    <- getscores(Dist2_1, dAB, A, B, C, dim = 2)
    end2    <- c(rn[A], rn[B])

    ### Dim 3
    Dist3   <- sapply(1:n, function(i) (scr2[pw$first[i]] - scr2[pw$second[i]]) ^ 2)
    Dist3_1 <- Dist3 + Dist2
    Dist3_2 <- cbind(pw$first, pw$second,
                     sqrt(ifelse(mat1[,3]^2-Dist3_1 < 0, 0, mat1[,3]^2-Dist3_1)))
    ep3     <- getendp(Dist3_2, RD = Dist3_2[, 3], dim = 3)
    A       <- ep3$A
    B       <- ep3$B
    C       <- ep3$C
    if (A < B) {
        dAB <- Dist3_2[, 3][Dist3_2[, 1] == A & Dist3_2[, 2] == B]
    }
    if (A > B) {
        dAB <- Dist3_2[, 3][Dist3_2[, 2] == A & Dist3_2[, 1] == B]
    }
    scr3    <- getscores(Dist3_2, dAB, A, B, C, dim = 3)
    end3    <- c(rn[A], rn[B])

    ### Dim 4 (residual)
    Dist4   <- sapply(1:n, function(i) (scr3[pw$first[i]] - scr3[pw$second[i]]) ^ 2)
    Dist4_1 <- Dist4 + Dist3 + Dist2
    Dist4_2 <- sqrt(ifelse(mat1[, 3]^2 - Dist4_1 < 0, 0, mat1[, 3]^2 - Dist4_1))

    ### results ----------------------------------------------------------------
    scores <- matrix(cbind(scr1, scr2, scr3), ncol=3, nrow=nr,
                     dimnames = list(rn, c('Dim1', 'Dim2', 'Dim3')))
    SSresid <- c(sum(Dist2_1[,3]^2), sum(Dist3_2[,3]^2), sum(Dist4_2^2))
    varexpl <- 100 * (1 - (SSresid / SStot))
    endpts  <- rbind(end1, end2, end3)
    sumtab  <- data.frame(SSresid, varexpl, endpts)
    dimnames(sumtab) <- list(c('Dim1','Dim2','Dim3'),
                             c('SS residuals','% var expl','endpt A','endpt B'))
    return(list(sumtab=sumtab, scores=scores))
}

####    END    ####
