# Description for file sortingSpices.rda
# Hervé Abdi: April 10, 2018.
#
#  sortingSpices Preambule ----
#' @title 62 assessors from 5 countries sort 16 spice samples
#' 
#' @description 
#' \code{sortingSpices}: 62 participants from 5 different countries
#' (USA, France, India, Spain, and Vietnam) for 16 different spices
#' (including 6 mixtures of spices).
#' 
#'  The data consist in
#' a list containing 7 objects
#' (for all sorting data the number 
#' at the intersection of a row and a colum
#' indicates the number of the pile in which the spice was sorted);):
#'  \code{}: A data frame containing the
#' 1) \code{sortAll}: A data frame containing the
#' results of the sorting task for
#' all 62 participants,
#' 2) \code{sortAmerican}: A data frame containing the
#'  results of the sorting task for
#' the 9 American participants,
#' 3) \code{sortFrench}: A data frame containing the
#'  results of the sorting task for
#' the 21 French participants,
#' 4) \code{sortIndian}: A data frame containing the
#'  results of the sorting task for
#' the 15 Indian participants,
#' 5) \code{sortSpanish}: A data frame containing the
#'  results of the sorting task for
#' the 11 Spanish participants,
#' 6) \code{sortVietnamese}: A data frame containing the
#'  results of the sorting task for
#' the 6 Vietnamese participants, and
#' 7) \code{spicesDescription} A data frame containing the
#' description of the Spices.
#' 
#' @details 
#' In the data frames, the spice blends are identifed
#' wiht acronyms that are expended in the data frame
#' \code{spicesDescription}.
#' @name sortingSpices
#' @usage data("sortingSpices")
#' @docType data
#' @format
#' A list containing 7 objects
#' (for all sorting data, the number 
#' at the intersection of a row and a colum
#' indicates the number of the pile in which the spice was sorted):
#'  \code{}: A data frame containing the
#' 1) \code{sortAll}: A data frame containing the
#' results of the sorting task for
#' all 62 participants,
#' 2) \code{sortAmerican}: A data frame containing the
#'  results of the sorting task for
#' the 9 American participants,
#' 3) \code{sortFrench}: A data frame containing the
#'  results of the sorting task for
#' the 21 French participants,
#' 4) \code{sortIndian}: A data frame containing the
#'  results of the sorting task for
#' the 15 Indian participants,
#' 5) \code{sortSpanish}: A data frame containing the
#'  results of the sorting task for
#' the 11 Spanish participants,
#' 6) \code{sortVietnamese}: A data frame containing the
#'  results of the sorting task for
#' the 6 Vietnamese participants, and
#' 7) \code{spicesDescription} A data frame containing the
#' description of the Spices.
#' @references
#' Part of these data (i.e., the French sample) is described 
#' and analyzed in 
#'  Chollet, S., Valentin, D., & Abdi, H. (2014). 
#'  Free sorting task. In P.V. Tomasco & G. Ares (Eds), 
#'  \emph{Novel Techniques in Sensory Characterization 
#'  and Consumer Profiling}. 
#'  Boca Raton: Taylor and Francis. pp 207-227.

#'@keywords datasets DistatisR
#'@author  Chollet, S., Valentin, D., & Abdi, H. 
NULL
# End of sortingSpices ----

#_____________________________________________________________________
# Print function sortingWines ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set: 
#' \code{sortingSpices}
#'
#' Change the print function for the data set: 
#' \code{sortingSpices} 
#' (class \code{dataSortingSpices})
#'
#' @param x a list: the data set: {sortingWines}
#' @param ... the rest
#' @author Herve Abdi
#' @export
print.dataSortingSpices <- function(x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 62 Assessors (from 5 countries) Sort 16 Spices   \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$sortAll            ","16 Spices * 62 Assessors: all sorting data.")
  cat("\n$sortAmerican       ","16 Spices *  9 Assessors: American data.")
  cat("\n$sortFrench         ","16 Spices * 21 Assessors: French data.")
  cat("\n$sortIndian         ","16 Spices * 15 Assessors: Indian data.")
  cat("\n$sortSpanish        ","16 Spices * 11 Assessors: Spanish data.")
  cat("\n$sortVietnamese     ","16 Spices *  6 Assessors: Vietnamese data.")
  cat("\n$spicesDescription ","data frame describing the spices.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.dataSortingSpices
# end print.dataSorting ----
#_____________________________________________________________________



  