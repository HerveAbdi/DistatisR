#' @title SUMPCAnormMat
#' @param Y A matrix to be SUM-PCA-normalized
#'
#' @return An SUM-PCA-normalized Y
#' @export
#'
#' @examples
#' SUMPCAnormMat(Y)
SUMPCAnormMat <- function (Y) {
    Ynormed <- Y / sqrt(sum(Y^2))
    return(Ynormed)
}
