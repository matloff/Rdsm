
# threads configuration: run
# rthreadsSetup(nThreads=2,
#    sharedVars=list(nextRowNum=c(1,1),m=c(10,100000000)))

setup <- function()  # run in "manager thread"
{
   # generate vectors to be sorted, of different sizes
   tmp <- c(30000000,70000000)
   nvals <- sample(tmp,10,replace=TRUE)  # 10 vectors to sort
   for (i in 1:10) {
      n <- nvals[i]
      m[i,1:(n+1)] <- c(n,runif(n))
   }
}

doSorts <- function()  # run in all threads
{

   rowNum <- myID  # my first vector to sort

   while (rowNum <= nrow(m)) {
      n <- m[rowNum,1]
      x <- m[rowNum,2:(n+1)]
      m[rowNum,2:(n+1)] <- sort(x)
      lock(mutex0)
      rowNum <- nextRowNum[1,1]
      nextRowNum[1,1] <- rowNum + 1
      unlock(mutex0)
   }

}
