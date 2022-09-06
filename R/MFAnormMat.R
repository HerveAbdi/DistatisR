#' @title MFAnormMat
#' @param Y A matrix to be MFA-normalized
#'
#' @return An MFA-normalized Y
#' @export
#'
#' @examples
#' ## a random 5 x 6 matrix
#' X <- matrix(rnorm(30), nrow = 5)
#' MFAnormMat(X)
MFAnormMat <- function (Y) {
    sv = svd(Y)
    sv1 = sv$d[1]
    Ynormed = Y/sv1
    return(Ynormed)
}
