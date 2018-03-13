#' Print S+ matrix results
#'
#' Print S+ matrix results.
#'
#'
#' @param x a list that contains items to make into the Splus class.
#' @param \dots inherited/passed arguments for S3 print method(s).
#' @author Derek Beaton
#' @keywords print
#' @export
print.Splus <-
function (x,...) {


	res.Splus <- x
	if (!inherits(res.Splus, "Splus")) stop ("no convenient data")
	cat("**Results for S+ Matrix**\n")
	cat("*The results are available in the following objects:\n\n")

	if(res.Splus$compact){
		#list(Splus=Splus)
		res <- array("", c(1, 2), list(1:1, c("name", "description")))
		res[1,] <- c("$Splus","S+ Matrix")
	}else{
		res <- array("", c(7, 2), list(1:7, c("name", "description")))
		res[1,] <- c("$SCP","Sum of Cross Products matrix")
		res[2,] <- c("$eigValues","Eigenvalues of Compromise")
		res[3,] <- c("$tau","Percentage of Explained Variance per Eigenvalue")
		res[4,] <- c("$F","Factor scores")
		res[5,] <- c("$PartialF","Partial Factor Scores")
		res[6,] <- c("$ProjectionMatrix", "The Projection Matrix")
		res[7,] <- c("$Splus","S+ Matrix")
	}
	print(res)
}
