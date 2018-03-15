# A scale function that can cope with all numbers in a vector 
# being equal (all numbers are then set to 0).
# 

#' \code{scale0}: A variation over the base r \code{scale} 
#' that avoids the "divide by 0 = NA"  problem.
#' 
#'@description \code{scale0}: A variation over the base 
#'\code{R} \code{scale} function
#'  \code{scale0}: centers (if needed) and scales a vector
#'  to norm 1; if the vector contains values all equal to a
#'  constant, \code{scale0} sets all values to 0
#'  (in lieu of NA like \code{scale} does). 
#'  Usefull when pre=processing tables
#'  for PCA-like type of analyses.
#'  
#'@param x a vector to be scaled
#'@param center (default = \code{TRUE}), when \code{TRUE}centers
#'  otherwise do nothing.
#'@return a centered (if required) and norm-1 normalized
#'  vector.
#'@author Herve Abdi
#'@examples
#'  toto   <- runif(10)     # 10 random number between 0 and 1
#'  tutu   <- scale0(toto)  # the same scaled and normalized
#'  toto0  <- rep(1,10)     # 10 numbers all equal to 1
#'  tutu0  <- scale0(toto0) # scaled to 0 # Compare with
#'  tutuNA <- scale(toto0)  # all numbers set to 0
#'@export

scale0 <- function(x, center = TRUE){
  # internal function
  # Uses near form dplyr but copy it to avoif loading dplyr
  near <- function(x, y, 
            tol = .Machine$double.eps^0.5) {abs(x - y) < tol}
  if (center) {z   <- x - mean(x)} else {z = x}
  normx <- sqrt(sum(z^2))
  if (!near(normx, 0) ){ z <- z / normx} else {z = z * 0}
  return(z)
}