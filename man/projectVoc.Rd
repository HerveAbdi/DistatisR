% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/projectVoc.R
\name{projectVoc}
\alias{projectVoc}
\title{Compute barycentric projections for count-like
description of the items of a \code{distatis}-type 
of  analysis.}
\source{
Abdi, H,  & Valentin, D. (2007). 
Papers available from
 \url{https://personal.utdallas.edu/~herve/}
}
\usage{
projectVoc(CT.voc, Fi, namesOfFactors = NULL)
}
\arguments{
\item{CT.voc}{a matrix or data.frame
storing a
 \eqn{I} items by \eqn{J} descriptors
 contingency table where the \eqn{i,j}-th cell 
 gives the number of times
 the \eqn{j}-th descriptor (in the column) 
 was used to describe the \eqn{i}-th item
 (in the row). \code{CT.voc} 
 needs to contain only non-negative 
 numbers.}

\item{Fi}{a matrix or data.frame
storing the
 \eqn{I} items by \eqn{L} factor scores obtained 
 from the compromise of a \code{distatis} 
 analysis or equivalent.}

\item{namesOfFactors}{(Default: NULL), if \code{NULL},
\code{projectVoc} uses the names of the columns of 
\code{Fi} for the names of the projected factors;
if \code{namesOfFactors} is one word then this word is used
to name the factors of the projections; 
if \code{namesOfFactors}
is a character vector, it is used to name the 
factors of
the projection.}
}
\value{
a list with 
1) \code{Fvoca.bary}: the barycentric projections of
the words,
and 2) \code{Fvoca.normed}: the CA normalized 
(i.e., variance of projections equals eigenvalue)
barycentric projections of
the words.
}
\description{
\code{projectVoc}
Compute barycentric projection for count-like
description of the items of a \code{distatis}-type of  analysis.
The data need to be non-negative and typically represent
the vocabulary (i.e., words) used to describe the items
in a sorting/ranking/projective-mapping task.
}
\details{
two types of projection are computed: 1)
a plain barycentric (words are positioned at the 
barycenter--a.k.a. center of mass--of 
the items it describes) and
2) a correspondence analysis barycentric 
where the variance
of the projected words is equal to the variance
of the items (as for correspondence 
analysis when using the
"symmetric" representation).
}
\examples{
# use the data from the BeersProjectiveMapping dataset
data("BeersProjectiveMapping")
# Create the I*J*K brick of data
zeBrickOfData <- projMap2Cube(
                  BeersProjectiveMapping$ProjectiveMapping, 
                  shape = 'flat',  nVars = 2)
# create the cube of covariance matrices between beers
cubeOfCov <- createCubeOfCovDis(zeBrickOfData$cubeOfData)
# Call distatis
testDistatis <- distatis(cubeOfCov$cubeOfCovariance, Distance = FALSE)
# Project the vocabulary onto the factor space
F4Voc <- projectVoc(BeersProjectiveMapping$CT.vocabulary, 
                    testDistatis$res4Splus$F)
}
\references{
Abdi, H., & Valentin, D., (2007). 
Some new and easy ways to describe, compare, 
and evaluate products and assessors. 
In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
\emph{New trends in sensory evaluation 
of food and non-food products}.
Ho Chi Minh (Vietnam): 
Vietnam National University & Ho Chi Minh City Publishing House. 
pp. 5-18.

 and
 
 Lahne, J., Abdi, H., & Heymann, H. (2018). 
 Rapid sensory profiles with DISTATIS and 
 barycentric text projection: An example with amari,
  bitter herbal liqueurs. 
  \emph{Food Quality and Preference, 66}, 36-43.
}
\author{
Herve Abdi
}
