
# threads configuration: run
# rthreadsSetup(nThreads=2)

setup <- function(preDAG)  # run in "manager thread"
{
   # to generate a DAG, take any data frame and run it through, say,
   # bnlearn:hc
   tmpadj <- amat(hc(preDAG))
   n <- nrow(tmpadj)
   rthreadsMakeSharedVar('adjm',nr,nr,info$infoDir,tmpadj)
   rthreadsMakeSharedVar('adjmPow',nr,nr,info$infoDir,tmpadj)
   rthreadsMakeSharedVar('iterNum',1,1,info$infoDir,1)

   read in the adjacency matrix from disk
   # generate vectors to be sorted, of different sizes
   tmp <- c(30000000,70000000)
   set.seed(9999)
   nvals <- sample(tmp,10,replace=TRUE)  # 10 vectors to sort
   for (i in 1:10) {
      n <- nvals[i]
      m[i,1:(n+1)] <- c(n,runif(n))
   }
}

findMinDists <- function()  # run in all threads, maybe with system.time()
{
   rthreadsAttachSharedVar

   tmp <- parallel::splitIndices(nr,info$nThreads)

   rowNum <- myID  # my first vector to sort

   while (rowNum <= nrow(m)) {
      # as illustration of parallel operation, see which threads execute
      # sorts on which rows
      print(rowNum)
      n <- m[rowNum,1]
      x <- m[rowNum,2:(n+1)]
      m[rowNum,2:(n+1)] <- sort(x)
      rowNum <- rthreadsAtomicInc('nextRowNum')
   }

   rthreadsWaitDone()

}
