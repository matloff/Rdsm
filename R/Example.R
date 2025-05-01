

setup <- function() 
{
   rdsmMakeSharedVar('nextRowNum',1,1,info$infoDir)
   rdsmMakeSharedVar('m',10,100000000,info$infoDir)
   tmp <- c(30000000,70000000)
   nvals <- sample(tmp,10,replace=TRUE)
   for (i in 1:10) {
      n <- nvals[i]
      m[i,] <- c(n,runif(n))
   }
}

doSorts <- function() 
{

   rowNum <- info$myID
   nextRowNum[1,1] <- info$nThreads + 1
   while (rowNum <= nrow(m) {
      n <- m[rowNum,1]
      x <- m[rowNum,2:(n+1)]
      m[rowNum,2:(n+1)] <- sort(x)
      lock('mutex0')
      rowNum <- nextRowNum[1,1]
      nextRowNum[1,1] <- rowNum + 
      unlock('mutex0')
   }

}
