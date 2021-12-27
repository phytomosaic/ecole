#' @title PCA with randomization test
#'
#' @description
#' Principal components analysis with randomization test for stopping rules.
#'
#' @param x array, where rows = SUs and cols = variables such as
#'    environmental or traits values
#'
#' @param B numeric, number of randomizations
#'
#' @param ... further arguments passed to other methods
#'
#' @return
#' List containing items returned by `stats::prcomp`, appended with further
#'     items:\cr
#'     - \code{eig}:  eigenvalues.\cr
#'     - \code{varexpl}:  proportion of variance explained.\cr
#'     - \code{cumvar}:  cumulative variance explained.\cr
#'     - \code{V}:  matrix of correlations between variables and the PCA scores.\cr
#'     - \code{tab}:  table of randomization results by PCA axis.\cr
#'     - \code{stopping}:  number of suggested dimensions, based on different
#'         stopping rules.
#'
#' @details
#' PCA with stopping rules based on RndLambda, RndF, AvgRnd, or broken-stick
#'    (Peres-Neto et al. 2005).  The current implementation hard-codes the
#'    cross-products matrix as a correlation matrix (i.e., data are scaled and
#'    centered).
#'
#' @references
#' Peres-Neto, P.R., D.A. Jackson, and K.M. Somers. 2005. How many principal
#'    components? stopping rules for determining the number of non-trivial axes
#'    revisited. Computational Statistics and Data Analysis 49:974–997.
#'
#' @examples
#' data(smoky)
#' pc <- ord_pca(smoky$env)                    # PCA with randomization
#' plot(pc$x, type='n', asp=1) ; text(pc$x, cex=0.7)  # plot first two axes
#' head(pc$tab)                                # better format than `summary()`
#' pc$stopping                                 # number of dimensions
#' pc$V                                        # V = correlations with PC axes
#'
#' @seealso \code{\link[stats]{prcomp}}
#'
#' @export
#' @rdname ord_pca
`ord_pca` <- function(x, B = 999, ...) {
        n      <- NROW(x)
        pc     <- stats::prcomp(x, center=TRUE, scale.=TRUE)
        pc$eig <- pc$sdev ^ 2
        pc$varexpl <- pc$eig / sum(pc$eig)
        pc$cumvar  <- cumsum(pc$varexpl)
        pc$V   <- t(apply(pc$rotation,1,function(a,b){a*b},pc$sdev))
        m      <- length(pc$eig)
        # U <- pc$rotation                                        # loadings
        # V <- t(apply(pc$rotation,1,function(a,b){a*b},pc$sdev)) # correlations
        # S <- pc$x                                               # scores
        `randomize_pca` <- function(x) {
                `matperm` <- function(x) {
                        pmat <- x
                        for (j in 1:m) { pmat[,j] <- x[sample(n,n,replace=FALSE),j] }
                        pmat
                }
                Lambda <- Fstat <- matrix(1e-6, B, m)
                rescum <- cumsum(sort(pc$eig, decreasing=FALSE))[1:(m-1)]
                Lambda[1,]       <- pc$eig
                Fstat[1,1:(m-1)] <- pc$eig[1:(m-1)] / sort(rescum,
                                                           decreasing=TRUE)
                for (b in 1:(B-1)) {
                        if(b %% 10 == 0) {cat(paste0(floor(b/B*100), "%... "))}
                        rndeig <- stats::prcomp(matperm(x),
                                                center=TRUE, scale.=TRUE)$sdev^2
                        if (length(rndeig) < m) {
                                rndeig <- c(rndeig, rep(1e-6, m - length(rndeig)))
                        }
                        rescum <- cumsum(sort(rndeig, decreasing=FALSE))[1:(m-1)]
                        Lambda[b+1, ] <- rndeig
                        Fstat[b+1, 1:(m-1)] <-
                                rndeig[1:(m-1)] / sort(rescum, decreasing=TRUE)
                }
                `calc_pval` <- function(v) { (sum(v >= v[1])) / (length(v)) }
                tab <- data.frame(
                        Axis        = 1:m,
                        VarExpl     = pc$varexpl,
                        CumVarExpl  = pc$cumvar,
                        Eigenvalue  = pc$eig,
                        MinRndEig   = apply(Lambda[-1,], 2, min),
                        MeanRndEig  = apply(Lambda[-1,], 2, mean),
                        MaxRndEig   = apply(Lambda[-1,], 2, max),
                        p_RndLambda = apply(Lambda, 2, calc_pval),
                        p_RndF      = apply(Fstat,  2, calc_pval)
                )
                tab[,2:7] <- sapply(tab[,2:7], round, digits=4)
                tab[,8:9] <- sapply(tab[,8:9], round, digits=nchar(B)+1)
                return(tab)
        }
        pc$tab      <- randomize_pca(x)
        keep_rndlam <- which(pc$tab$p_RndLambda > 0.05)[1] - 1
        keep_rndf   <- which(pc$tab$p_RndF > 0.05)[1] - 1
        keep_avgrnd <- which(pc$tab$Eigenvalue < pc$tab$MeanRndEig)[1] - 1
        bs          <- rev(cumsum(1/(m:1)) / m)
        keep_bs     <- which(pc$varexpl - bs < 0.000001)[1] - 1
        pc$stopping <- rbind(`Rnd-Lambda`   = keep_rndlam,
                             `Rnd-F`        = keep_rndf,
                             `Avg-Rnd`      = keep_avgrnd,
                             `broken-stick` = keep_bs)
        colnames(pc$stopping) <- 'Number of dimensions'
        cat('\n')
        print(pc$stopping)
        return(pc)
}
