#' @title Peirce function for inital classification of character data
#' 
#' @description 
#' \code{Peirce.char} evalutaes a data.frame for duplicated rows that represent Peirce's triads
#'    
#' @param  DF object of class data.frame with dim [n,3]
#' @return The number of duplicated rows, if any, and the identity of those rows
#'
#' @examples


Peirce.char <- function(DF) {    
  count.duplicates <- function(DF){
    x <- do.call('paste', c(DF, sep ='\r'))
    ox <- order(x)
    r1 <- rle(x[ox])
    cbind(DF[ox[cumsum(r1$lengths)],,drop=FALSE], count = r1$lengths)
  }
  
  dups <- duplicated(DF, incomparable=FALSE)
  
  if (!any(dups[]==TRUE)) {
    print("No duplicate rows")
    
  } else {
    
    dups2 <- count.duplicates(DF)
    
  }
  
    
  
}