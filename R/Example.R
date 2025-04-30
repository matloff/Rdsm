

setup <- function() 
{
   rdsmMakeSharedVar('nextRowNum',1,1,info$infoDir)
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
