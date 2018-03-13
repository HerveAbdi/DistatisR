# This file
# Describes the Ranking 6 Wines Data
# Created on March 12 2018 by Herve Abdi
# part of the DistatisR package.
#

#' \code{Ranking6WinesRawData}: an example of an excel file
#' with (simulated) ranking data. Can be read with the function
#' \code{read.df.excel()}.
#'
#'  \code{Ranking6WinesRawData}: an example of an excel file
#' with (simulated) ranking data (6 wines ranked by 80 Assessors).
#' Can be read by
#' \code{read.df.excel}.
#'
#' @details
#' In this example of a "ranking task" 
#' 80 (simulated or fictitious) 
#' assessors ranked 6 red wines ffrom Burgundy (France). 
#' The assessor first chooses
#' the most relevant dimension for these wines and then
#' positions the wines on a scale from 1 to 9 for this dimension.
#' The names of the assessors is composed of 4 characters
#' of the general composition {w/m}{a/f}{01 : 80}.
#' The assessors were 40 men and 40 women (first character w/n)
#' and 40 American or 40 French (second character a/f).
#' The red wines that were tasted are Irancy, Saint-Brie,
#' Beaune, Nuits (Cote de Nuits), Beaujolias, and Beaujolais-Nouveau.
#' 
#' @section Availability:
#' To fetch this dataset use \code{system.file()}
#' (see example below).
#' @name Ranking6WinesRawData
#' @section FileName: Ranking6WinesRawData.xlsx
#' @author Herve Abdi
#' @examples
#' path2file <- system.file("extdata",
#'            "Ranking6WinesRawData.xlsx", package = 'DistatisR')
#' ranking6Wines <- read.df.excel(path = path2file, sheet = 'Ranking')
NULL

