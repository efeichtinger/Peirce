#' Sample data from Twitter during the airing of an episode of AMC's The Walking Dead
#' 
#' @docType data
#' 
#' @usage data(Twitter)
#' 
#' @format An object of class data.frame
#' 
#' @keywords datasets
#' 
#' @examples
#' data(Twitter)
#' 
#' reprows <- Peirce.char(Twitter)
#' reprows
#' List of 2
#' $ : int 19
#' $ :'data.frame':	12 obs. of  4 variables:
#'  ..$ Sign        : Factor w/ 83 levels "abbydreed","absept22",..: 15 19 19 19 19 43 75 79 83 83 ...
#' ..$ Object      : Factor w/ 50 levels "albertovargas94",..: 46 45 46 47 48 46 46 46 45 46 ...
#' ..$ Interpretant: Factor w/ 4 levels "Mentions","None",..: 1 1 1 1 1 1 1 1 1 1 ...
#' ..$ count       : int [1:12] 2 3 3 3 2 2 2 2 2 2 ...