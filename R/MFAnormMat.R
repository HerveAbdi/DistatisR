MFAnormMat <- function (Y) {
    sv = svd(Y)
    sv1 = sv$d[1]
    Ynormed = Y/sv1
    return(Ynormed)
}
