#' @title Peirce sign function for inital classification of character data
#' 
#' @description 
#' \code{charPeirce} evalutaes a data.frame for duplicated rows that represent Peirce's triads
#'    
#' @param  DF object of class data.frame with dim [n,3]
#' @return list with the index of the first duplicated row, if any, 
#' and duplicated rows, the identity of those rows and how many times
#' each row is duplicated, excludes rows that are unique 
#'
#' 
#'
#' @examples
#' Peirce.char(x)
#' "No duplicate rows"
#' 
#' head(twitter)
#' 
#' Peirce.char(twitter)
#' [[1]]   19
#' 


charPeirce <- function(DF) {    
  count.duplicates <- function(DF){
    x <- do.call('paste', c(DF, sep ='\r'))
    ox <- order(x)
    r1 <- rle(x[ox])
    cbind(DF[ox[cumsum(r1$lengths)],,drop=FALSE], count = r1$lengths)
  }
  
  dups <- duplicated(DF, incomparable=FALSE)
  fspot <- anyDuplicated(DF)
  
  if (!any(dups[]==TRUE)) {
    print("No duplicate rows")
    
  } else {
    
    dups2 <- count.duplicates(DF)
    dups3 <- subset(dups2, dups2$count >= 2)
    
    z <- list(fspot, dups3)
    return(z)
  }
  
}