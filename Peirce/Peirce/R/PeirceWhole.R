#' @title Peirce Function for whole numbers 
#' 
#' @description
#' \code{Peirce.whl} returns a data frame with the average distribution
#' 
#' 
#' @param DF object of class data.frame with 3 columns 
#' @return a data frame with the average distribution for each of 3 variables
#' 
Peirce.whl <-  function(DF)
{
  
  check.numeric <- function(N){
    !length(grep("[^[:digit:]]", as.character(N)))
  }
  
  col1 = DF[,1];
  col2 = DF[,2];
  col3 = DF[,3];
  
  if(check.numeric(col1))
  {
    DF$percentageofx = ecdf(col1)(col1);
  }
  
  if(check.numeric(col2))
  {
    DF$percentageofy = ecdf(col2)(col2);
  }
  
  if(check.numeric(col3))
  {
    DF$percentageofz = ecdf(col3)(col3);
  }
  
  xbool = FALSE;
  ybool = FALSE;
  zbool = FALSE;
  
  if("percentageofx" %in% colnames(DF))
  {
    if(check.numeric(col1))
    {
      x = DF$percentageofx;
      xbool = TRUE;
    }
  }
  
  if("percentageofy" %in% colnames(DF))
  {
    if(check.numeric(col2))
    {
      y = DF$percentageofy;
      ybool = TRUE;
    }
  }
  
  if("percentageofz" %in% colnames(DF))
  {
    if(check.numeric(col3))
    {
      z = DF$percentageofz;
      zbool = TRUE;
    }
  }
  
  
  for(i in 1:nrow(DF))
  { 
    if(xbool && ybool && zbool)
    {
      DF$Total[i] = (x[i]+y[i]+z[i])/3
    }
    
    if(xbool && ybool)
    {
      DF$Total[i] = (x[i]+y[i])/2
    }
    
    if(xbool && zbool)
    {
      DF$Total[i] = (x[i]+z[i])/2
    }
    
    if(ybool && xbool)
    {
      DF$Total[i] = (y[i]+x[i])/2
    }
    
    if(ybool && zbool)
    {
      DF$Total[i] = (y[i]+z[i])/2
    }
     
  }
  
  View(DF);
}