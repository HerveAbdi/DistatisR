#' \code{distatis} 3-Way MDS based on the \acronym{STATIS} optimization
#' procedure.
#'
#'  \code{distatis}: Implements the \acronym{DISTATIS}
#'  method which is a 3-way generalization of
#' metric multidimensional scaling
#' (\emph{a.k.a.} classical MDS or principal
#' coordinate analysis).
#' \code{distatis} takes a set of \eqn{K} distance
#' (or covariance)
#' matrices describing a set of \eqn{I} observations and computes (1) a set of
#' factor scores that describes the similarity structure of the distance
#' matrices (e.g., what distance matrices describe the observations in the same
#' way, what distance matrices differ from each other) (2) a set of factor
#' scores (called the \emph{compromise} factor scores) for the observations
#' that best describes the similarity structure of the observations and (3)
#' partial factor scores that show how each individual distance matrix "sees"
#' the compromise space.  \code{distatis} computes the compromise as an optimum
#' linear combination of the cross-product matrices associated to each distance
#' matrix.  \code{distatis} can also be applied to a set of covariance
#' matrices.
#'
#' \acronym{DISTATIS} is part of the \acronym{STATIS} family.
#'  It is often used
#' to analyze the results of sorting tasks.
#'
#' @aliases distatis DiSTATIS CovSTATIS covstatis
#' @param LeCube2Distance an "observations \eqn{\times}{*} observations
#' \eqn{\times}{*} distance matrices" array of dimensions \eqn{I\times I \times
#' K}{I*I*K}.  Each of the \eqn{K} "slices" is a \eqn{I\times I}{I*I} square
#' distance (or covariance) matrix describing the \eqn{I} observations.
#' @param Norm Type of normalization used for each cross-product matrix derived
#' from the distance (or covariance) matrices.  Current options are \code{NONE}
#' (do nothing) or \code{MFA} (\code{default} ) that normalizes each matrix so
#' that its first eigenvalue is equal to one.
#' @param Distance if \code{TRUE} (\code{default}) the matrices are distance matrices,
#' if \code{FALSE} they are covariance matrices.
#' @param RV if \code{TRUE} (\code{default}) we use the \eqn{R_V}{Rv} coefficient to
#' compute the \eqn{\alpha}{weights}, if \code{FALSE}
#' we use the matrix scalar product
#' @param nfact2keep Numner of factors to keep for the computation of the
#' factor scores of the observations.
#' @param compact if \code{FALSE} (default),
#'  \code{distatis} provides detailled output, if
#'  \code{TRUE},  \code{distatis} sends back
#' only the \eqn{\alpha}{alpha} weights (this option is used to make the
#' bootstrap routine \code{\link{BootFromCompromise}} more efficient).
#' @return
#' \code{distatis} sends back the results \emph{via} two lists:
#' \code{res.Cmat}
#' and \code{res.Splus}.
#' Note that items with a * are the only ones sent back
#' when using the \code{compact = TRUE} option.
#' \item{res.Cmat}{Results for the between distance matrices analysis.}
#' \itemize{ \item \code{res.Cmat$C} The \eqn{I\times I}{I*I} \bold{C} matrix
#' of scalar products (or \eqn{R_V}{Rv} between distance matrices).  \item
#' \code{res.Cmat$vectors} The eigenvectors of the \bold{C} matrix \item
#' \code{res.Cmat$alpha} * The \eqn{\alpha}{alpha} weights \item
#' \code{res.Cmat$value} The eigenvalues of the \bold{C} matrix \item
#' \code{res.CmatG} The factor scores for the \bold{C} matrix }
#' \item{res.Splus}{Results for the between observation analysis.}
#' \itemize{
#' \item \code{res.Splus$SCP} an \eqn{I\times I\times K}{I*I*K} array.
#' Contains
#' the (normalized if needed)
#' cross product matrices corresponding to the
#' distance matrices.
#' \item \code{res.Splus$Splus} * The compromise (linear
#' combination of the SCP's')
#' \item \code{res.Splus$ProjectionMatrix} The
#' projection matrix used to compute factor
#' scores and partial factor scores.
#' \item \code{res.Splus$F} The factor scores for the observations.
#' \item \code{res.Splus$PartialF} an
#' \eqn{I\times \code{nf2keep} \timesK}{I*nf2keep*K} array.
#' Contains the partial factors for the distance
#' matrices.}
#' @author Herve Abdi
#' #@seealso \code{\link{GraphDistatisAll}} \code{\link{GraphDistatisBoot}}
#' #\code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}}
#' #\code{\link{GraphDistatisRv}} \code{\link{DistanceFromSort}}
#' #\code{\link{BootFactorScores}} \code{\link{BootFromCompromise}}
#' #as \code{\link{help}},
#' @references Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005).
#' DISTATIS: The analysis of multiple distance matrices.  \emph{Proceedings of
#' the IEEE Computer Society: International Conference on Computer Vision and
#' Pattern Recognition}.  (San Diego, CA, USA). pp. 42--47.
#'
#' Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007).  Analyzing
#' assessors and products in sorting tasks: DISTATIS, theory and applications.
#' \emph{Food Quality and Preference}, \bold{18}, 627--640.
#'
#' Abdi, H., Dunlop, J.P., & Williams, L.J. (2009).  How to compute reliability
#' estimates and display confidence and tolerance intervals for pattern
#' classiffers using the Bootstrap and 3-way multidimensional scaling
#' (DISTATIS).  \emph{NeuroImage}, \bold{45}, 89--95.
#'
#' Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012).  STATIS
#' and DISTATIS: Optimum multi-table principal component analysis and three way
#' metric multidimensional scaling.  \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics}, \bold{4}, 124--167.
#'
#' The \eqn{R_V} coefficient is described in
#'
#' Abdi, H. (2007). RV coefficient and congruence coefficient.  In N.J. Salkind
#' (Ed.): \emph{Encyclopedia of Measurement and Statistics}.  Thousand Oaks
#' (CA): Sage. pp. 849--853.
#'
#' Abdi, H. (2010). Congruence: Congruence coefficient, RV coefficient, and
#' Mantel Coefficient.  In N.J. Salkind, D.M., Dougherty, & B. Frey (Eds.):
#' \emph{Encyclopedia of Research Design.} Thousand Oaks (CA): Sage. pp.
#' 222--229.
#'
#' (These papers are available from \url{www.utdallas.edu/~herve})
#' @keywords distatis mds
#' @examples
#'
#' # 1. Load the DistAlgo data set (available from the DistatisR package)
#' data(DistAlgo)
#' # DistAlgo is a 6*6*4 Array (face*face*Algorithm)
#' #-----------------------------------------------------------------------------
#' # 2. Call the DISTATIS routine with the array of distance (DistAlgo) as parameter
#' DistatisAlgo <- distatis(DistAlgo)
#' @export
distatis <-
function(LeCube2Distance, Norm = 'MFA', Distance = TRUE,
                     RV = TRUE, nfact2keep = 3,compact = FALSE){
# DISTATIS
# Implement the standard DISTATIS program
# described in Abdi H. et al, 2005, 2007, 2009, & 2012.
# (References to be completed)
# The compact option is used for the bootstrap

# Private functions
# DblCenterDist Create the Centering Matrix
DblCenterDist <- function(Y){ # Double Center a distance matrix
	nI = nrow(Y)
    CentMat = diag(nI) - (1/nI)*matrix(1,nI,nI)
	S = -(1/2)*(CentMat%*%Y%*%CentMat)
	return(S)
} # end of DblCenterDist
#
# *************************************************************************************************
Dist2CP <- function(D3){ # Transform a Cube of Distance into a cube of CP
CP3 <- (array(apply(D3,3,DblCenterDist),dim=c(dim(D3)[1],dim(D3)[2],dim(D3)[3])))
dimnames(CP3) <- dimnames(D3)
return(CP3)
} # end of Dist2CP
#
# *************************************************************************************************
# NormMFA4CP Normalize a CP matrix product to first eigenvalue of one
MFAnormCP <- function(Y){# MFA Normalize a psd matrix (i.e., first eigenvalue=1)
	ev = eigen(Y,symmetric=T,only.values=T)[1]
	e1 = ev$values[1]
	Ynormed = Y/e1
	return(Ynormed)
} # End of MFAnormed
#
# *************************************************************************************************
# CP2MFAnormedCP: MFA normalize a cube of CP
  CP2MFAnormedCP<- function(CP3){ # Transform a cube of CP into an MFA normed cube of CP
  CP3normed <- array(apply(CP3,3,MFAnormCP),dim=dim(CP3))
  dimnames(CP3normed) <- dimnames(CP3)
  return(CP3normed)
}
# *************************************************************************************************
# Compute the RV coefficient matrix
#
GetCmat <- function(CubeCP, RV = TRUE){# Compute a C matrix (or RV) from a cube of CP
	# get the dimensions
	nI = dim(CubeCP)[1]
	nJ = dim(CubeCP)[3]
	# reshape the 3D array as an (nI^2)*(nJ) matrix
	CP2 =  array(CubeCP,dim=c(nI*nI,nJ))
	C = t(CP2)%*%CP2 # Scalar Product
	if (RV){ # RV is TRUE we want the RV coefficient instead of the Scalar Product
		   laNorm = sqrt(apply(CP2^2,2,sum))
		    C = C / ( t(t(laNorm))%*%laNorm)
        } # end if
    rownames(C) <- dimnames(CubeCP)[[3]] -> colnames(C)
    return(C)
}
#
# *************************************************************************************************
# Compute the compromise
ComputeSplus <- function(CubeCP,alpha){# Compute the comprise matrix for STATIS/DISTATIS
	nJ = dim(CubeCP)[3];
	nI = dim(CubeCP)[1]
	Splus = matrix(0,nI,nI)
	# Former Horrible Loop. Changed in final version
	# for (i in 1:nJ){Splus = Splus + alpha[i]*CubeCP[,,i]}
	# A better way
	Splus <- apply( apply(CubeCP,c(1,2),'*',t(alpha)),c(2,3),sum)
	return(Splus)
    } # end ComputeSplus
# End of Private Routines
# *************************************************************************************************
  # Transform the cube of distances into a cube of Cross-Products
  # if Distance is already a covariance or correlation reverse the sign
  if  (Distance != TRUE) {LeCube2Distance = -LeCube2Distance}
  # double center
  CP3 = Dist2CP(LeCube2Distance)
  # perform MFA normalization
  if (Norm == 'MFA') {CP3 <- CP2MFAnormedCP(CP3)}
  # Maybe more options here in the future)
  # Compute the C matrix as an RV matrix
  #
  C = GetCmat(CP3,RV=RV) # (to match matlab implementation of distatis)
                         # get the RV coefficient mat
                         # instead of the scalar product

  # eigen of C
  eigC <- eigen(C,symmetric=TRUE) # Eigen of C

  if (compact==FALSE){
     eigC$vectors[,1] <- abs(eigC$vectors[,1])
     rownames(eigC$vectors) <- rownames(C)
     nfC = ncol(eigC$vectors) # number of eigenvectors of C
     Nom2Dim = paste('dim',1:nfC)
     colnames(eigC$vectors) <- Nom2Dim
     # make sure that first eigenvector is positive
     eigC$tau <- round(100*(eigC$values / sum(eigC$values)))
     names(eigC$tau)    <- Nom2Dim
     names(eigC$values) <- Nom2Dim
     # factors scores for RV mat
     eigC$G = t(apply(eigC$vectors,1,'*',t(t(sqrt(abs(eigC$values))))))
     rownames(eigC$G) <- rownames(C)
     colnames(eigC$G) <- Nom2Dim
  }
  # alpha weights
  alpha = eigC$vectors[,1] / sum(eigC$vectors[,1])
  # compute compromise
  Splus = ComputeSplus(CP3,alpha)
  if (compact == FALSE){
     # Eigen decomposition of Splus
     eigenSplus = eigen(Splus,symmetric=TRUE)
     # Percent Of Inertia
     eigenSplus$tau = round(100*eigenSplus$values / sum(eigenSplus$values))
     # singular values
     eigenSplus$SingularValues = sqrt(abs(eigenSplus$values))
     # Get the factor scores (ugly way, but works)
     F <- t(apply(eigenSplus$vectors,1,'*',t(t(eigenSplus$SingularValues))))
     rownames(F) <- rownames(Splus)
     # Keep only the interesting factors
     Nom2Factors = paste('Factor',1:nfact2keep)
     F <- F[,1:nfact2keep]
     colnames(F) <- Nom2Factors
     # Projection matrix
     ProjMat <- t(apply(eigenSplus$vectors,1,'*',1/t(t(eigenSplus$SingularValues))))
     Proj = ProjMat[,1:nfact2keep]
     colnames(Proj) <- Nom2Factors
     rownames(Proj) <- rownames(Splus)
    # Get the partial projections
    PartialF = array(apply(CP3,3,'%*%',Proj),
       dim=c(dim(CP3)[1],nfact2keep,dim(CP3)[3])    )
    rownames(PartialF) <- rownames(Splus)
    colnames(PartialF) <- Nom2Factors
    dimnames(PartialF)[[3]] <- rownames(C)

    # pack up the information to send back
    # Will try to Keep a structure similar to  Cherise Chin Fatt MExPosition
    # in the meantime go for fast and match matlab
     	 res.Cmat <- list(C = C, eigVector = eigC$vector,eigValues = eigC$values,
                   tau = eigC$tau, G = eigC$G, alpha=alpha, compact=compact)
         class(res.Cmat) <- c("Cmat","list")
    	 res.Splus <- list(SCP = CP3,F = F, PartialF = PartialF, ProjectionMatrix = Proj,Splus=Splus, compact=compact)
         class(res.Splus) <- c("Splus","list")
		res.distatis <- list(res4Cmat= res.Cmat,res4Splus =res.Splus, compact=compact)
		class(res.distatis) <- c("DistatisR","list")
     } # End of if compact == FALSE
     else {# What do you do when it TRUE send back only the compact information
    	 res.Cmat <- list(alpha=alpha, compact=compact)
         class(res.Cmat) <- c("Cmat","list")
	     res.Splus <- list(Splus=Splus, compact=compact)
         class(res.Splus) <- c("Splus","list")
		 res.distatis <- list(res4Cmat= res.Cmat,res4Splus =res.Splus, compact=compact)
		class(res.distatis) <- c("DistatisR","list")
     }

     return(res.distatis)
}
