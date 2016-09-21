## Peirce functions
library(compare)

Peirce.int <-  function(DF)
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
    ##puts total percentage value
    
    
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
  
  
  
  ##Creates a percentage rank of values based on the column they reside in
  
  
  
  View(DF);
  
  ##Reading the data column, 50% means a distribution that is very close to average.
}

#Test - simulation 
x <- c(0,1,0,1,1,1,0,0,1,0)
y <- c(95,3,97,5,4,4,98,97,4,98)
z <- c(3,10,2,10,11,11,2,3,10,2)
df <- data.frame(x,y,z)

x1 <- floor(runif(100, min=0, max=101))
y1 <- floor(runif(100, min=0, max=101))
z1 <- floor(runif(100, min=0, max=101))
df1 <- data.frame(x1,y1,z1)

test1 <- Peirce.int(df)
test2 <- Peirce.int(df1)


###Peirce character function
#### 9 20 2016 - this compares two rows and returns T or F for entire row

#then returns 3 T or F for each column position 
#Put this function within the character function? Is there a way to iteratre?
check.match <- function(df){
  x <- compare(df[1,],df[2,])
  return(x)
}

#Made up character strings for testing character function
x2 <- c("cat","cat","cat","dog","dog","dog")
y2 <- rep(LETTERS[1:6],1)
z2 <- c("a","b","c","d","e","f")
df2 <- data.frame(x2,y2,z2)

twitter <- read.csv("Twitter_Peirce.csv")
class(twitter)
head(twitter)
str(twitter)

# returns a logical value for match of entire row
# also returns logical values for each column position 
check.match(df2)
check.match(twitter)
