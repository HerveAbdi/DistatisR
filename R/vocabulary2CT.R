
#---------------------------------------------------------------------
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(vocabulary2CT)
#
#---------------------------------------------------------------------

#' @title Transforms a data.frame of product by vocabulary 
#' of assessors into
#' a  products by words (from vocabulary) contingency table.
#' 
#' @description \code{vocabulary2CT}
#'  Transforms a data.frame of products by vocabulary 
#' of assessors into
#' a  products by words (from vocabulary) contingency table.
#' In  contingency table the number at the intersection
#'  of a row (product) and a column (word) is the number
#'  of assessors who used this word to describe that product.
#' 
#' @param df.voc a data frame with the vocabulary. In this 
#' data.frame 
#' each element 
#' stores
#' the words used by one assessor to describe a beer 
#' (words are separated with spaces);
#' @return \code{CT.vocabulary} a matrix storing the 
#' prodcuts by words contingency table.
#' @examples 
#' # Get BeersProjectiveMapping example
#' data("BeersProjectiveMapping")
#' aContingenyTable <- vocabulary2CT(BeersProjectiveMapping$Vocabulary)
#' 
#' @seealso 
#'  \code{\link[tidytext]{unnest_tokens}}
#'  \code{\link[dplyr]{count}}
#'  \code{\link{vocabulary2CT}}
#' @rdname vocabulary2CT
#' @export 
#' @import dplyr
#' @import tidytext
#' @import magrittr
#' @import tibble
#---------------------------------------------------------------------
# A function to transform the vocabulary into a contingency table
#
# needs tidytext 
vocabulary2CT <- function(df.voc){
  # parameters needed: df.voc
  # Transform the df.voc into a list
  Jk <- list()
  nK <- ncol(df.voc)
  for (k in 1:nK){
    Jk[[k]] <- df.voc[,k]
  }
  names(Jk) <-  colnames(df.voc) # paste0('J_',1:7)
  #-------------------------------------------------------------------
  nI   <- length(Jk[[1]])
  nK   <- length(Jk)

  if (is.null(row.names(df.voc) )) {
    namesOfProducts <- paste0('P',1:nI)
  } else {
    namesOfProducts  <- row.names(df.voc)  
  }
  J_p <-  unlist(Jk) # In order to get the vocabulary
  df4voc.tmp <- data_frame(line = 1:(nI*nK), text = as.character(J_p))
  words = NULL # these two line to appease the parser that
  text = NULL  # thnks that words and text are undefined 
  # when used as parameters
  tmp1 <-  unnest_tokens(df4voc.tmp, "words", "text",format = 'text')
  frequency <- count(tmp1, words, sort = FALSE)
  # for some strange reason the pipe creates problems 
  # so we are back to explicit temporary assigments
  # frequency <- df4voc.tmp %>% 
  #       tidytext::unnest_tokens("words", "text", format = 'text') %>% 
  #       count(words, sort = FALSE)
  # 
  Vocabulary <- frequency$words
  nVoc <- length(Vocabulary)
  namesOfJudges <- colnames(df.voc)
  if (!is.null(namesOfJudges)) namesOfJudges = names(Jk) 
  # Test twice in case the last has no names
  if (is.null(namesOfJudges)){namesOfJudges <- paste0('J',1:nK)}
  cubeOfVocabulary <- array(0, dim = c(nI, nVoc, nK) )
  dimnames(cubeOfVocabulary)[[1]] <- namesOfProducts
  dimnames(cubeOfVocabulary)[[2]] <- Vocabulary
  dimnames(cubeOfVocabulary)[[3]] <- namesOfJudges
  #print('before loop')
  for (k in 1:nK){
    for (i in  1:nI){
      wordsInVoc <- Vocabulary %in% unnest_tokens(
                   data_frame(line = 1, 
                   text =  as.character(Jk[[k]][[i]])),words,text)$words
      cubeOfVocabulary[i,,k] <- wordsInVoc
    } # end of loop in i
  } # end of loop in k
  CT.vocabulary <- apply(cubeOfVocabulary,c(1,2),sum)
  return(CT.vocabulary)
}
#---------------------------------------------------------------------