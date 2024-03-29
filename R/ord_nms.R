#' @title NMS with randomization test and stepdown dimensionality selection
#'
#' @description
#' Nonmetric multidimensional scaling with randomization test for stopping rules
#'     and stepdown dimensionality selection.
#'
#' @param x array, where rows = SUs and cols = variables such as
#'    environmental or traits values.
#'
#' @param autopilot character, one of three possible thoroughness options.
#'
#' @param method dissimilarity index, passed to `vegan::vegdist`.
#'
#' @param parallel number of cores to use in parallel processing.  Set `NULL` to
#'     avoid parallel processing.
#'
#' @param ... further arguments passed to `vegan::metaMDS`.
#'
#' @return
#' List containing items returned by `vegan::metaMDS` for the selected model,
#'    appended with further items:\cr
#'     - \code{pval}:  permutation p-values for each dimensionality.\cr
#'     - \code{stress_real_vs_rnd}:  stress values, where each column =
#'     dimensionality, and each row = either the real-data stress value
#'     (first row only) or the randomized-data stress value (successive rows).\cr
#'
#' @details
#' NMS with stopping rules for dimensionality based on PC-ORD
#'     (McCune and Mefford 2017).  This is time-consuming, use parallel
#'     processing where possible.
#'
#' @references
#' McCune, B., and M. J. Mefford. 2017. PC-ORD. Multivariate Analysis
#'     of Ecological Data. Version 7. MjM Software Design, Gleneden
#'     Beach, OR.
#'
#' @examples
#' \dontrun{
#' data(smoky)
#' m1 <- ord_nms(smoky$spe, 'quick')
#' m2 <- ord_nms(smoky$spe, 'medium')
#' m3 <- ord_nms(smoky$spe, 'slow')
#' }
#' @seealso \code{\link[vegan]{metaMDS}}
#'
#' @export
#' @rdname ord_nms
`ord_nms` <- function(x,
                      autopilot = c('quick', 'medium', 'slow'),
                      method = 'bray',
                      parallel = parallel::detectCores() - 1,
                      ...) {
    ### setup
    if (inherits(x, 'dist')) stop('`x` must not be `dist` object')
    D   <- vegan::vegdist(x, method=method)
    opt <- c('quick-and-dirty', 'medium', 'slow-and-thorough')
    i   <- pmatch(autopilot, opt)[1]
    p   <- data.frame(dims     = c(3, 4, 6),
                      try      = c(10, 50, 250),
                      nrand    = c(19, 49, 249),
                      maxit    = c(100, 200, 500))[i,]
    k        <- p$dims
    try      <- p$try
    nrand    <- p$nrand
    maxit    <- p$maxit
    cat('---------------------------------------------------\n')
    cat('-----------   Autopilot NMS now working   ---------\n')
    cat('---------------------------------------------------\n\n')
    cat(paste0('start time: ', time_start <- Sys.time()), '\n')
    cat('you chose `', opt[i], '` autopilot\n')
    cat('searching 1 thru', k, 'dimensions\n')
    cat('number of runs with real data:', try, '\n')
    cat('number of runs with randomized data:', nrand, '\n')
    cat('maximum iterations per run:', maxit, '\n')
    ### real data runs
    real_stress <- sapply(1:k, function(kk) {
        cat('real data:', kk, 'of', k, 'dimensions... ')
        m  <- vegan::metaMDS(D, k=kk, try=try, maxit=maxit, trace=0, ...)
        cat('stress =', m$stress, '\n')
        m$stress
    })
    #### shuffle within columns, then get dissimilarity matrix
    `perm` <- function(x) {
        x     <- as.matrix(x)
        nr    <- NROW(x)
        empty <- TRUE
        maxit <- 20
        itr   <- 0
        while (any(empty) & itr <= maxit) {
            itr   <- itr + 1
            p     <- apply(x, 2, function(j) j[sample(nr)] )
            empty <- which(rowSums(p) == 0)
            if(any(empty)) cat('permutation',itr,'had empty rows\n')
        }
        rownames(p) <- rownames(x)
        return(vegan::vegdist(p, method = attributes(D)$method))
    }
    # ### randomization data runs (TODO: permit parallel processing)
    # cat('randomization: ')
    # rnd_stress <- sapply(1:nrand, function(rand) {
    #     cat(rand, 'of', nrand, '...  ')
    #     stress <- sapply(1:k, function(kk) {
    #         m  <- vegan::metaMDS(perm(x), k=kk, try=1, trymax=1,
    #                              maxit=maxit, trace=0, ...)
    #         m$stress
    #     })
    #     return(stress)
    # })
    ### randomization data runs (enables parallel processing)
    cat('beginning randomizations...\n ')
    `f` <- function (nrand, parallel, ...) {
        if (is.null(parallel)) { parallel <- 1 }
        hasClus     <- inherits(parallel, 'cluster')
        isParal     <- hasClus || parallel > 1
        isMulticore <- .Platform$OS.type == 'unix' && !hasClus
        if (isParal && !isMulticore && !hasClus) {
            parallel <- parallel::makeCluster(parallel)
            parallel::clusterEvalQ(parallel, library(vegan))
        }
        nclus <- ifelse(hasClus, length(parallel), parallel)
        `g` <- function(...) {
            sapply(1:k, function(kk) {
                vegan::monoMDS(perm(x), k=kk, maxit=maxit, ...)$stress
            })
        }
        if (isParal) {
            if (isMulticore) {
                s <- simplify2array(
                    parallel::mclapply(1:nrand, function(i) g(), mc.cores = nclus)
                )
            } else {
                s <- simplify2array(
                    parallel::parLapply(parallel, 1:nrand, function(i) g() )
                )
            }
        } else {
            s <- sapply(1:nrand, function(i) g())
        }
        if (isParal && !isMulticore && !hasClus) {
            parallel::stopCluster(parallel)
        }
        return(s)
    }
    rnd_stress <- f(nrand=nrand, parallel=parallel)
    ### randomization pvalues
    stress           <- rbind(real_stress, t(rnd_stress))
    rownames(stress) <- c('real',paste0('rnd_',1:nrand))
    pval <- apply(as.matrix(stress), 2, function(x) {
        num  <- sum(x[-1] <= x[1]) + 1
        den  <- length(x[-1]) + 1
        num/den
    })
    ### plotting
    `f` <- function(stress) {
        x    <- stress
        ymin <- min(x, na.rm=TRUE) - 0.05
        ymax <- max(x, na.rm=TRUE) + 0.02
        par(bty='l', las=1, pty='s')
        boxplot(x[-1,], ylim=c(ymin, ymax), boxwex=0.2, staplewex=0.1, lty=1,
                outcex=NA, xlab='Dimensions', ylab='Stress',
                main='Randomization vs Observed Stress')
        points(x[1,], pch=16, col=2)
        lines(x[1,], lty=1, col=2)
        text(1:3, ymin, paste0('pval = ', round(pval, 4)), cex=0.75)
        legend('topright', leg=c('Randomized','Real'), bty='n', bg=NA,
               border=NA, fill=c('#00000050','#DF536B'), cex=0.75)
    }
    try(f(stress), silent=TRUE)
    ### apply final selected model
    is_sig     <- if (nrand > 0) pval <= 0.05 else TRUE
    is_improve <- c(TRUE, abs(diff(real_stress)) > 0.05)
    k_final    <- which(is_improve & is_sig)[-1][1]
    if(is.na(k_final)) { k_final <- 1 }
    m_final <- vegan::metaMDS(D, k=k_final, try=try, maxit=maxit, trace=0, ...)
    m_final$pval               <- pval
    m_final$stress_real_vs_rnd <- stress
    class(m_final)  <- c('ord_nms', class(m_final))
    ### print results
    cat('\n\n---------------------------------------------------\n')
    cat('-------------- Autopilot NMS results --------------\n')
    cat('---------------------------------------------------\n\n')
    cat('Measures of fit for final model:\n')
    print(data.frame('...........' = fitstats_nms(m_final)))
    cat('\nfinal selected dimensionality:', m_final$ndim, '\n')
    cat('improvement, by dimension:', abs(c(1,diff(real_stress))), '\n')
    cat('permutational p-values, by dimension:', pval, '\n')
    tab <- table(D)
    cat('number of tie blocks in dissimilarity matrix:', sum(tab > 1), '\n')
    cat('number of elements involved in ties:', sum(tab[tab > 1]), '\n')
    cat('number of elements in dissimilarity matrix:', sum(tab), '\n')
    `noshare` <- function(x) {
        z <- vegan::no.shared(x)
        sum(z) / length(z)
    }
    cat('proportion of no-share sample units:', round(noshare(x),3), '\n')
    print(m_final)
    cat(paste0('end time:   ',   time_end <- Sys.time()), '\n')
    cat('time elapsed: ',
        round(as.numeric(difftime(time1 = time_end,
                                  time2 = time_start, units = 'mins')), 3),
        'minutes\n')
    return(m_final)
}
### unexported helper
`screeplot_nms` <- function(object, ...) {
    stopifnot(inherits(object, 'ord_nms'))
    x    <- as.matrix(object$stress_real_vs_rnd)
    nc   <- NCOL(x)
    pval <- object$pval
    ymin <- min(x, na.rm=TRUE) - 0.05
    ymax <- max(x, na.rm=TRUE) + 0.02
    par(bty='l', las=1, pty='s')
    boxplot(x[-1,], ylim=c(ymin, ymax), boxwex=0.2, staplewex=0.1, lty=1,
            outcex=NA, xlab='Dimensions', ylab='Stress',
            main='Randomization vs Observed Stress', ...)
    points(x[1,], pch=16, col=2)
    lines(x[1,], lty=1, col=2)
    text(1:nc, ymin, paste0('pval = ', round(pval, 4)), cex=0.75)
    legend('topright', leg=c('Randomized','Real'), bty='n', bg=NA, border=NA,
           fill=c('#00000050','#DF536B'), cex=0.75)
}
### unexported helper
`fitstats_nms` <- function (object) {
    # setup
    x  <- object$diss
    y  <- object$dist
    yf <- object$dhat
    # R2n, nonmetric fit
    rstress <- 1 - object$stress^2
    # R2l, linear fit (monotonic fit)
    ralscal <- 0
    if (object$iregn > 1) {
        if (object$iregn == 3) {
            k <- seq(object$istart[2], object$ndis)
            ralscal <- cor(y[k], yf[k])^2
        }
        else {
            ralscal <- cor(y, yf)^2
        }
    }
    if (object$iregn != 2) {
        ist <- c(object$istart, object$ndis + 1)
        if (object$iregn == 3)
            object$ngrp <- 1
        for (j in 1:object$ngrp) {
            k <- seq(ist[j], ist[j + 1] - 1)
            ralscal <- ralscal + cor(y[k], yf[k])^2
        }
    }
    ralscal <- ifelse(object$iregn == 3, ralscal/2, ralscal/object$ngrp)
    # R2m, metric fit AKA variance explained
    rmetric <- cor(x,y) ^ 2
    # output
    out <- c('Non-metric fit, R2n' = rstress,
             'Linear fit, R2l' = ralscal,
             'Metric fit, R2m' = rmetric)
    return(out)
}
