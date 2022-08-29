#' @title MFAnormMat
#' @param Y A matrix to be MFA-normalized
#'
#' @return An MFA-normalized Y
#' @export
#'
#' @examples
#' MFAnormMat(Y)
MFAnormMat <- function (Y) {
    sv = svd(Y)
    sv1 = sv$d[1]
    Ynormed = Y/sv1
    return(Ynormed)
}
