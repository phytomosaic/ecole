#' @title Bray-Curtis ordination, aka polar ordination
#'
#' @description Bray-Curtis ordination.
#'
#' @param x array, where rows = SUs and cols = variables such as environmental
#'   or traits values.
#'
#' @param method dissimilarity index, passed to `vegan::vegdist`.
#'
#' @param endpoint Endpoint selection method. `'BC.original'` is Bray-Curtis
#'   original of selecting most-distant SUs.  `'PC_ORD.original'` is the
#'   deprecated method from PC-ORD, whereby the first endpoint has the highest
#'   sum of distances from other sites, and the second endpoint has the highest
#'   distance from the first endpoint. `'var.reg'` is the variance regression
#'   method now default in PC-ORD (McCune and Mefford 2017).
#'
#' @param ends Numeric, index two sample units as endpoints on first axis.
#'
#' @param ... further arguments not currently passed to other functions.
#'
#' @return List containing items:\cr
#' - \code{SSresid}:  sums-of-squares of residual distances per axis.\cr
#' - \code{varexplained}:  variance explained per axis.\cr
#' - \code{endpoints}:  endpoint sample units per axis.\cr
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
#' o1 <- ord_bc(spe, method='bray', endpoint='BC.original')
#' o2 <- ord_bc(spe, method='bray', endpoint='BC.original', ends=c(1,12))
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
                     endpoint = c('var.reg', 'BC.original', 'PC_ORD.original'),
                     ends = c(NA,NA),
                     # get.resid.dist = FALSE,
                     ...) {
    # Original author: Ken Aho, https://sites.google.com/a/isu.edu/aho/
    # tarball:  https://drive.google.com/file/d/0B65xY-gnYYGramVGcURnZGIyZGM/
    # Modified: Rob Smith, modified from `plant.ecol::polar.ord()`
    z <- c('var.reg', 'BC.original', 'PC_ORD.original')
    endpoint  <- z[pmatch(endpoint, z)]
    if (inherits(x, 'dist')) {
        diss  <- x
    } else {
        diss  <- vegan::vegdist(x, method)
    }
    rn    <- attr(diss, 'Labels')
    nr    <- attr(diss, 'Size')
    if(nr > 250) {
        cat('  current implementation version works best for n < 500;\n',
            '    e.g., n = 500 takes ~10 minutes!')
        x <- c(20, 50, 100, 150, 200, 250)            # n SUs
        y <- c(0.035, 0.32, 2.12, 9.59, 30.05, 71.7)  # seconds
        ecole::set_par(1)
        ecole::plot_loglog(log10(x),log10(y),xlim=c(10,1000),ylim=c(0.01,2500),
                           ylab='Time (seconds)', xlab='Number of sample units',
                           main='Expected run time (1-5 min in red band)',
                           cex.main=1.0)
        cf <- coef(lm(log10(y)~log10(x)))
        abline(cf)
        rect(log10(1), log10(60*1),
             log10(450), log10(60*5), col='#FF000020', border=NA) # 1 to 5 min
        rect(log10(250), log10(0.001),
             log10(450), log10(60*5), col='#FF000020', border=NA)
    }
    D     <- as.matrix(as.dist(diss, upper = TRUE, diag = TRUE))
    n     <- (nr^2 - nr) / 2 # num of elements, same as `length(diss)`
    SStot <- sum(diss ^ 2)
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
    if (endpoint == 'BC.original') {
        `getendp` <- function(mat, RD, dim = 1) {
            endp <- mat[mat[, 3] == max(RD), ]
            if (!is.null(nrow(endp))) {
                A <- as.numeric(endp[1, ][1])
                B <- as.numeric(endp[1, ][2])
            }
            if (is.null(nrow(endp))) {
                A <- as.numeric(endp[1])
                B <- as.numeric(endp[2])
            }
            C     <- seq(1:nr)
            C     <- C[seq(1:nr) != B & seq(1:nr) != A]
            res   <- list()
            res$A <- A
            res$B <- B
            res$C <- C
            res
        }
    }
    else if (endpoint == 'PC_ORD.original') {
        `getendp` <- function(mat, RD, dim = 1) {
            if (dim == 1) {
                endD <- getDmat(mat)
                endV <- apply(endD, 2, sum)
                A <- as.numeric(which(endV == max(endV)))[1]
                B <- as.numeric(which(endD[, A] == max(endD[,A])))[1]
            }
            if (dim > 1) {
                endp <- mat[mat[, 3] == max(RD), ]
                if (!is.null(nrow(endp))) {
                    A <- as.numeric(endp[1, ][1])
                    B <- as.numeric(endp[1, ][2])
                }
                if (is.null(nrow(endp))) {
                    A <- as.numeric(endp[1])
                    B <- as.numeric(endp[2])
                }
            }
            C     <- seq(1:nr)
            C     <- C[seq(1:nr) != B & seq(1:nr) != A]
            res   <- list()
            res$A <- A
            res$B <- B
            res$C <- C
            res
        }
    }
    else if (endpoint == 'var.reg') {
        `getendp` <- function(mat, RD, dim = 1) {
            endD <- getDmat(mat)
            diag(endD) <- NA
            `sum.sq` <- function(x) {
                sum((x[!is.na(x)] - mean(x, na.rm=TRUE))^2)
            }
            endV <- apply(endD, 2, function(x) sum.sq(x))
            A    <- as.numeric(which(endV == max(endV)))[1]
            notA <- seq(1:nrow(endD))[seq(1:nrow(endD)) != A]
            Bmat <- matrix(ncol = 1, nrow = nrow(endD) - 1)
            diag(endD) <- 0
            for (i in 1:(nrow(endD) - 1)) {
                D1      <- endD[, A]
                Bmat[i] <- as.numeric(lm(endD[, notA[i]] ~ D1)$coefficients[2])
            }
            B     <- notA[Bmat == min(Bmat)][1]
            C     <- seq(1:nrow(endD))
            C     <- C[seq(1:nrow(endD)) != B & seq(1:nrow(endD)) != A]
            res   <- list()
            res$A <- A
            res$B <- B
            res$C <- C
            res
        }
    }
    `getscores` <- function(pw, D.AB, A, B, C, dim = 1) {
        scr <- matrix(ncol = 1, nrow = nr)
        for (i in 2:(nr - 1)) {
            scr[1] <- 0
            scr[nr] <- D.AB
            if      (C[i-1] > A & C[i-1] > B) {
                scr[i] <-
                    (D.AB^2 +
                         (pw[,3][pw[,1] == A & pw[,2] == C[i-1]]^2) -
                         (pw[,3][pw[,1] == B & pw[,2] == C[i-1]]^2)) / (2*D.AB)
            }
            else if (C[i-1] < A & C[i-1] > B) {
                scr[i] <-
                    (D.AB^2 +
                         (pw[,3][pw[,2] == A & pw[,1] == C[i-1]])^2 -
                         (pw[,3][pw[,1] == B & pw[,2] == C[i-1]]^2)) / (2*D.AB)
            }
            else if (C[i-1] < A & C[i-1] < B) {
                scr[i] <-
                    (D.AB^2 +
                         (pw[,3][pw[,2] == A & pw[,1] == C[i-1]])^2 -
                         (pw[,3][pw[,2] == B & pw[,1] == C[i-1]]^2)) / (2*D.AB)
            }
            else if (C[i-1] > A & C[i-1] < B) {
                scr[i] <-
                    (D.AB^2 +
                         (pw[,3][pw[,1] == A & pw[,2] == C[i-1]])^2 -
                         (pw[,3][pw[,2] == B & pw[,1] == C[i-1]]^2)) / (2*D.AB)
            }
        }
        sites   <- c(A, C, B)
        scr <- as.matrix(scr[order(sites), ])
        colnames(scr) <- paste('Dim', dim, 'scores')
        scr
    }
    Dist1 <- matrix(ncol = 1, nrow = n)
    for (i in 1:n) {
        Dist1[i] <- D[pw$second[i], (pw$first[i])]
    }
    mat1 <- cbind(pw$first, pw$second, Dist1)
    ### allow Axis 1 to have subjective endpoints
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
    ### resume
    if (A < B) {
        D.AB <- mat1[, 3][mat1[, 1] == A & mat1[, 2] == B]
    }
    if (A > B) {
        D.AB <- mat1[, 3][mat1[, 2] == A & mat1[, 1] == B]
    }
    scores1 <- getscores(mat1, D.AB, A, B, C, dim = 1)
    end1    <- c(rn[A], rn[B])
    Dist2   <- matrix(ncol = 1, nrow = n)
    for (i in 1:n) {
        Dist2[i] <- as.matrix(scores1[pw$first[i]] -
                                  scores1[pw$second[i]])^2
    }
    Dist2.1 <- cbind(pw$first,
                     pw$second,
                     sqrt(ifelse(mat1[,3]^2-Dist2 < 0, 0, mat1[,3]^2-Dist2)))
    ep2 <- getendp(Dist2.1, RD = Dist2.1[, 3], dim = 2)
    A   <- ep2$A
    B   <- ep2$B
    C   <- ep2$C
    if (A < B) {
        D.AB <- Dist2.1[, 3][Dist2.1[, 1] == A & Dist2.1[, 2] == B]
    }
    if (A > B) {
        D.AB <- Dist2.1[, 3][Dist2.1[, 2] == A & Dist2.1[, 1] == B]
    }
    scores2 <- getscores(Dist2.1, D.AB, A, B, C, dim = 2)
    end2    <- c(rn[A], rn[B])
    Dist3   <- matrix(ncol = 1, nrow = n)
    for (i in 1:n) {
        Dist3[i] <- as.matrix(scores2[pw$first[i]] -
                                  scores2[pw$second[i]])^2
    }
    Dist3.1 <- Dist3 + Dist2
    Dist3.2 <- cbind(pw$first, pw$second,
                     sqrt(ifelse(mat1[,3]^2-Dist3.1 < 0,0,mat1[,3]^2-Dist3.1)))
    ep3 <- getendp(Dist3.2, RD = Dist3.2[, 3], dim = 3)
    A   <- ep3$A
    B   <- ep3$B
    C   <- ep3$C
    if (A < B) {
        D.AB <- Dist3.2[, 3][Dist3.2[, 1] == A & Dist3.2[, 2] == B]
    }
    if (A > B) {
        D.AB <- Dist3.2[, 3][Dist3.2[, 2] == A & Dist3.2[, 1] == B]
    }
    scores3 <- getscores(Dist3.2, D.AB, A, B, C, dim = 3)
    Dist4   <- matrix(ncol = 1, nrow = n)
    for (i in 1:n) {
        Dist4[i] <- as.matrix(scores3[pw$first[i]]-scores3[pw$second[i]])^2
    }
    Dist4.1 <- Dist4 + Dist3 + Dist2
    Dist4.2 <- sqrt(ifelse(mat1[, 3]^2 - Dist4.1 < 0, 0, mat1[, 3]^2 - Dist4.1))
    end3    <- c(rn[A], rn[B])
    ### results
    res      <- list()
    res$SSresid <- matrix(
        data = c(sum(Dist2.1[,3]^2), sum(Dist3.2[,3]^2), sum(Dist4.2^2)),
        ncol = 1, nrow = 3,
        dimnames = list(c('Dim1','Dim2','Dim3'), c('SS residuals')))
    # if (isTRUE(get.resid.dist)) {
    #     res$DistResid <- list(
    #         orig.dist = as.dist(D),
    #         axis2     = as.dist(getDmat(Dist2.1)),
    #         axis3     = as.dist(getDmat(Dist3.2)))
    # }
    res$varexplained <- matrix(
        data = 100 * (1 - (res$SSresid / SStot)),
        ncol = 1, nrow = 3,
        dimnames = list(c('Dim1','Dim2','Dim3'), c('% Var expl')))
    res$endpoints <- as.data.frame(matrix(
        data = rbind(end1, end2, end3),
        ncol = 2, nrow = 3,
        dimnames = list(c('Dim1', 'Dim2', 'Dim3'), c('Endpt1', 'Endpt2'))))
    res$scores    <- matrix(
        data = cbind(scores1, scores2, scores3),
        ncol = 3, nrow = nr,
        dimnames = list(rn, c('Dim1', 'Dim2', 'Dim3')))
    res
}

####    END    ####
