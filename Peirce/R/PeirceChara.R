#' @title Peirce function for characters
#' 
#' @description 
#' \code{Peirce.char} returns logical values, TRUE or FALSE, indicating exact 
#'    matches among rows at each column position and logical values for each
#'    position
#'   
#'    
#' @param  DF object of class data.frame with 3 columns
#' @return logical table TRUE or FALSE for an exact match for entire row
#'  and a logical value for each column position 
#'  
#' @examples
#' #Check for matches between 2 rows, matches at all three positions 
#' and matches at each position  
#' df[1:2,1:3]
#'    x2  y2  z2
#'  1 cat  A  a
#'  2 cat  B  b
#'  Peirce.char(df)
#'  FALSE [TRUE, FALSE, FALSE]


Peirce.char <- function(DF){
  x <- compare(DF[1,],DF[2,])
  return(x)
}