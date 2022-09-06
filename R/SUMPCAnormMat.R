#' @title SUMPCAnormMat
#' @param Y A matrix to be SUM-PCA-normalized
#'
#' @return An SUM-PCA-normalized Y
#' @export
#'
#' @examples
#' ## a random 5 x 6 matrix
#' X <- matrix(rnorm(30), nrow = 5)
#' MFAnormMat(X)
SUMPCAnormMat <- function (Y) {
    Ynormed <- Y / sqrt(sum(Y^2))
    return(Ynormed)
}
