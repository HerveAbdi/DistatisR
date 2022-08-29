# Preamble ----
#_____________________________________________________________________
# statis: function statis
# Private functions used in this file
#  >>> See file names matching the function names
# -- private functions
#   o DblCenterDist 
#   o Dist2CP       
#   o MFAnormCP
#   o CP2MFAnormedCP
#   o GetCmat
#   o ComputeSplus
#   o rdiag & ldiag
#________________________
# Last update  07 / 28 / 2022 by Ju-Chi
#_____________________________________________________________________
# statis Preamble ----
#' multitable method with  "STATIS" optimization
#' procedure for asymmetric, rectangular matrices
#' @title statis
#' @param LeListOfTables an "observations 
#' \eqn{\times}{*} observations
#' \eqn{\times}{*} distance matrices" array of dimensions
#' \eqn{I\times I \times  K}{I*I*K}.
#' Each of the \eqn{K} "slices" is a \eqn{I\times I}{I*I} square
#' distance (or covariance) matrix describing the 
#' \eqn{I} observations.
#' @param Norm Type of normalization 
#' used for each cross-product matrix derived
#' from the distance (or covariance) matrices.  
#' Current options are \code{NONE}
#' (do nothing), \code{SUMPCA} (normalize by the total inertia) 
#' or \code{MFA} (\code{default}) that normalizes each matrix so
#' that its first eigenvalue is equal to one.
#' @param Distance if \code{TRUE} (\code{default}) 
#' the matrices are distance matrices, \code{FALSE}
#' the matrices are treated as positive semi-definite matrices
#' (e.g., scalar products,
#' covariance, or correlation matrices).
#' @param double_centering if \code{TRUE} 
#' (\code{default}) the matrices are double-centered
#' (should always be used for distances).
#' if \code{FALSE} the matrices
#' will \emph{not} be double centered 
#' (note that these matrices 
#' should be semi positive definite matrices such that
#' covariance matrices).
#' @param RV if \code{TRUE} (\code{default}) 
#' we use the \eqn{R_V}{Rv} coefficient to
#' compute the \eqn{\alpha}{weights}, 
#' if \code{FALSE}
#' we use the matrix scalar product
#' @param nfact2keep (default: \code{3}) Number of factors 
#' to keep for the computation of the
#' factor scores of the observations.
#' @param compact if \code{FALSE} (default),
#'  \code{distatis} provides detailed output, if
#'  \code{TRUE},  \code{distatis} sends back
#' only the \eqn{\alpha}{alpha} weights
#'  (this option is used to make the
#' bootstrap routine 
#' \code{\link{BootFromCompromise}} more efficient).
#'
#' @return  \code{statis} sends back the results 
#' \emph{via} two lists:
#' \code{res.Cmat}
#' and \code{res.Splus}.
#' Note that items with a * are the only ones sent back
#' when using the \code{compact = TRUE} option.
#' 
#' \item{res.Cmat}{Results for the between 
#'           distance matrices analysis.}
#' \itemize{ 
#'   \item \code{res.Cmat$C}
#'   The \eqn{I\times I}{I*I} \bold{C} matrix
#'   of scalar products (or \eqn{R_V}{Rv} between matrices).  
#'   \item
#'   \code{res.Cmat$vectors} The eigenvectors of the \bold{C} matrix 
#'   \item
#'  \code{res.Cmat$alpha} * The \eqn{\alpha}{alpha} weights 
#'  \item
#'  \code{res.Cmat$value} The eigenvalues of the \bold{C} matrix 
#'  \item
#'  \code{res.Cmat$G} The factor scores for the \bold{C} matrix 
#'  \item 
#'   \code{res.Cmat$ctr} The contributions for \code{res.Cmat$G},
#'  \item
#'    \code{res.Cmat$cos2} The squared cosines for \code{res.Cmat$G}
#'  \item
#'  \code{res.Cmat$d2} The squared 
#'  Euclidean distance  for \code{res.Cmat$G}.
#'    }
#'    
#' \item{res.Splus}{Results for the between observation analysis.}
#' \itemize{
#' \item \code{res.Splus$SCP} an \eqn{I\times I\times K}{I*I*K} array.
#' Contains
#' the (normalized if needed)
#' cross product matrices corresponding to the
#' distance matrices.
#' \item \code{res.Splus$Splus} * The compromise 
#' (optimal linear
#' combination of the SCP's').
#'  \item \code{res.Splus$eigValues} *
#'   The eigenvalues of the compromise).
#'  \item \code{res.Splus$eigVectors} *
#'   The eigenvectors of the compromise).
#' \item \code{res.Splus$tau} * The percentage
#' of explained inertia of the eigenValues).
#' \item \code{res.Splus$ProjectionMatrix} The
#' projection matrix used to compute factor
#' scores and partial factor scores.
#' \item \code{res.Splus$Fi} The factor scores for the observations.
#' \item \code{res.Splus$Fj} The factor scores for the variables.
#'  \item 
#'   \code{res.Splus$ctr.i} The contributions for \code{res.Cmat$Fi}.
#'   \item 
#'   \code{res.Splus$ctr.j} The contributions for \code{res.Cmat$Fj}.
#'  \item
#'    \code{res.Splus$cos2.i} The squared cosines for \code{res.Cmat$Fi}.
#'  \item
#'  \code{res.Splust$d2.i} The squared 
#'  Euclidean distance  for \code{res.Cmat$Fi}.
#'  \item
#'  \code{res.Splust$d2.j} The squared 
#'  Euclidean distance  for \code{res.Cmat$Fj}.
#' \item \code{res.Splus$PartialFi} an
#' \eqn{I \times \code{nf2keep} \times K}{I*nf2keep*K} array.
#' Contains the partial factors for the distance
#' matrices.}
#' 
#' @examples
#' add(1, 1)
#' add(10, 1)
statis <- function(LaGrandeTable,
                   DESIGN,
                Norm = 'MFA',
                center = TRUE, 
                scale = "SS1",
                RV = TRUE,
                nfact2keep = 3,
                compact = FALSE) {
    
    # data('wines2012')
    # design=c('NZ','NZ','NZ','NZ','FR','FR','FR','FR','CA','CA','CA','CA')
    
    ## center and scale the data tables
    LaGrandeTable.preproc <- lapply(LaGrandeTable, function(x) expo.scale(x, center = center, scale = scale))
    
    # perform MFA normalization ----
    if (Norm == 'MFA') {
        LaGrandeTable.preproc <- lapply(LaGrandeTable.preproc, MFAnormMat)
    }
    if (Norm == 'SUMPCA') {
        LaGrandeTable.preproc <- lapply(LaGrandeTable.preproc, SUMPCAnormMat)
    }
    
    ## Put data into an array
    if (is.list(LaGrandeTable.preproc)){
        diff.row <- lapply(LaGrandeTable.preproc, nrow) %>% unique %>% length()
        diff.col <- lapply(LaGrandeTable.preproc, ncol) %>% unique %>% length()
        
        if (diff.row == 1 & diff.col > 1 | diff.row == 1 & diff.col == 1 & diff.row < diff.col){
            ## create crossproduct with rows
            data.list <- lapply(LaGrandeTable.preproc, function(x) crossprod(t(x)))
        }else if (diff.row > 1 & diff.col == 1 | diff.row == 1 & diff.col == 1 & diff.row > diff.col){
            data.list <- lapply(LaGrandeTable.preproc, crossprod)
        }else{
            stop("Either the rows or the columns of the data tables have to match.")
        }
        leCube <- array(as.numeric(unlist(data.list)), 
                        dim=c(dim(data.list[[1]])[1], dim(data.list[[1]])[2], length(data.list)),
                        dimnames = list(rownames(data.list[[1]]), colnames(data.list[[1]]), names(data.list)))
    }else if(is.array(LaGrandeTable.preproc)){
        leCube <- LaGrandeTable.preproc
    }else{
        stop("The tables should be in a list or an array.")
    }
    
    # Compute C matrix ----
    C <- GetCmat(leCube, RV = RV) # (to match matlab implementation of distatis)
    
    # eigen of C ----
    eigC <- eigen(C, symmetric = TRUE) # Eigen-decomposition of C
    
    if (compact == FALSE) {
        # All C stuff ----
        eigC$vectors[, 1] <- abs(eigC$vectors[, 1])
        rownames(eigC$vectors) <- rownames(C)
        nfC <- ncol(eigC$vectors) # number of eigenvectors of C
        Nom2Dim <- paste('dim', 1:nfC)
        colnames(eigC$vectors) <- Nom2Dim
        # make sure that first eigenvector is positive
        eigC$tau <- round(100 * (eigC$values / sum(eigC$values)))
        names(eigC$tau)    <- Nom2Dim
        names(eigC$values) <- Nom2Dim
        # factors scores for RV mat
        eigC$G   <- t(apply(eigC$vectors, 1, '*', t(t(sqrt(abs(eigC$values) )))))
        rownames(eigC$G) <- rownames(C)
        colnames(eigC$G) <- Nom2Dim
        G2        <- eigC$G^2
        # new C stuff ----
        eigC$ctr  <- rdiag(G2, 1/eigC$values)  
        eigC$d2G  <- rowSums(G2)
        eigC$cos2 <- ldiag(1/eigC$d2G, G2) 
    }
    # alpha weights ----
    alpha <- eigC$vectors[, 1] / sum(eigC$vectors[, 1])
    # compute compromise ----
    Splus <- ComputeSplus(leCube, alpha)
    if (compact == FALSE) {
        # S+ stuff ----
        # eigen of S+ ----
        # Eigen decomposition of Splus
        eigenSplus <- eigen(Splus, symmetric = TRUE)
        # Percent Of Inertia
        eigenSplus$tau <- round(100 * 
                                    eigenSplus$values / sum(eigenSplus$values))
        # singular values
        eigenSplus$SingularValues <- sqrt(abs(eigenSplus$values))
        # Get the factor scores (ugly way, but works)
        Fi <- t(apply(eigenSplus$vectors, 1, '*', t(t(
            eigenSplus$SingularValues ))))
        rownames(Fi) <- rownames(Splus)
        # Add on ctr + cos.
        # new S stuff----
        Fi2     <-  Fi^2
        d2Fi    <-  rowSums(Fi2)
        ctrFi   <- rdiag(Fi2, 1/ abs(eigenSplus$values) )
        cos2Fi  <- ldiag(1/d2Fi, Fi2)
        # Keep only the interesting factors
        Nom2Factors <- paste('Factor', 1:nfact2keep)
        Fi <- Fi[, 1:nfact2keep]
        colnames(Fi) <- Nom2Factors
        ctrFi            <- ctrFi[, 1:nfact2keep]
        colnames(ctrFi)  <- Nom2Factors
        cos2Fi           <- cos2Fi[, 1:nfact2keep] 
        colnames(cos2Fi) <- Nom2Factors
        # Projection matrix ----
        ProjMat <- t(apply(eigenSplus$vectors, 1, '*', 
                           1 / t(t(eigenSplus$SingularValues))))
        Proj <- ProjMat[, 1:nfact2keep]
        colnames(Proj) <- Nom2Factors
        rownames(Proj) <- rownames(Splus)
        # Get the partial projections
        # PartialF ----
        PartialFi <- array(apply(leCube, 3, '%*%', Proj),
                          dim = c(dim(leCube)[1], nfact2keep, dim(leCube)[3]))
        rownames(PartialFi) <- rownames(Splus)
        colnames(PartialFi) <- Nom2Factors
        dimnames(PartialFi)[[3]] <- rownames(C)
        # Column factor scores ----
  
        Q <- lapply(LaGrandeTable.preproc, function(x){
            qj <- t(x) %*% eigenSplus$vectors[,1:nfact2keep] %*% diag(1/eigenSplus$SingularValues[1:nfact2keep])
            rownames(qj) <- rownames(x)
            colnames(qj) <- Nom2Factors
            return(qj)
        })
        
        Fj_noalpha <- lapply(LaGrandeTable.preproc, function(x){ ## missing alpha 
            fj <- t(x) %*% eigenSplus$vectors[,1:nfact2keep]
            rownames(fj) <- colnames(x)
            colnames(fj) <- Nom2Factors
            return(fj)
        })
        Fj <- mapply('*', Fj_noalpha, sqrt(alpha), SIMPLIFY = FALSE)

        Fj2     <-  lapply(Fj, '^', 2)
        d2Fj    <-  lapply(Fj, rowSums)
        ctrFj   <- lapply(Fj2, function(x) rdiag(x, 1/ abs(eigenSplus$values[1:nfact2keep])))
        cos2Fj  <- list()
        for (ln in 1:length(d2Fj)){
            cos2Fj[[ln]] <- ldiag(1/d2Fj[[ln]], Fj2[[ln]])
        }
        names(cos2Fj) <- names(Fj2)
        # pack up the information to send back.
        # May try as some point to keep a structure similar 
        # to MExPosition
        # in the meantime go for fast and match original matlab program.
        # pack return list ----
        res.Cmat <- list(
            C = C,
            eigVector = eigC$vector,
            eigValues = eigC$values,
            tau = eigC$tau,
            G = eigC$G,
            ctr  = eigC$ctr,
            cos2 = eigC$cos2,
            d2   = eigC$d2G,
            alpha = alpha,
            compact = compact
        )
        class(res.Cmat) <- c("Cmat", "list")
        res.Splus <- list(
            SCP = leCube,
            eigValues  = eigenSplus$values,
            eigVectors = eigenSplus$vectors,
            tau   = eigenSplus$tau,
            Fi = Fi,
            Fj = Fj,
            Q = Q,
            ctr.i = ctrFi,
            ctr.j = ctrFj,
            cos2.i = cos2Fi,
            cos2.j = cos2Fj,
            d2.i = d2Fi,
            d2.j = d2Fj,
            PartialFi = PartialFi,
            ProjectionMatrix = Proj,
            Splus = Splus,
            compact = compact
        )
        class(res.Splus) <- c("Splus", "list")
        res.statis <- list(res4Cmat = res.Cmat,
                             res4Splus = res.Splus,
                             compact = compact,
                             params = list(
                                 Norm = Norm,
                                 RV = RV))
        class(res.statis) <- c("StatisR", "list")
        # End of if compact == FALSE
    } else {
        # When "compact" is TRUE, send back only the compact information
        res.Cmat <- list(alpha = alpha, compact = compact)
        class(res.Cmat) <- c("Cmat", "list")
        res.Splus <- list(Splus = Splus, compact = compact)
        class(res.Splus) <- c("Splus", "list")
        res.distatis <- list(res4Cmat = res.Cmat,
                             res4Splus = res.Splus,
                             compact = compact)
        class(res.distatis) <- c("StatisR", "list")
    }
    # return ----
    return(res.statis) # et voila ----
}