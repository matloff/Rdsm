
# threads configuration: run
# rthreadsSetup(nThreads=2)

setup <- function(preDAG,destVertex)  # run in "manager thread"
{
   library(bnlearn)
   # to generate a DAG, take any data frame and run it through, say,
   # bnlearn:hc
   adj <- amat(hc(preDAG))
   n <- nrow(adj)
   rthreadsMakeSharedVar('adjm',n,n,initVal=adj)
   rthreadsMakeSharedVar('adjmPow',n,n,initVal=adj)
   # if row i = (u,v) is not (0,0) then it means the path search ended
   # after iteration u; v = 1 means reached the destination, v = 2
   # means no paths to destination exist
   rthreadsMakeSharedVar('done',n,2,initVal=rep(0,2*n))
   rthreadsMakeSharedVar('imDone',1,1,initVal=0)
   rthreadsMakeSharedVar('dstVrtx',1,1,initVal=destVertex)
   return()
}

findMinDists <- function(destVertex)  
   # run in all threads, maybe with system.time()
{
   if (myID > 0) {
      rthreadsAttachSharedVar('adjm')
      rthreadsAttachSharedVar('adjmPow')
      rthreadsAttachSharedVar('done')
   }
   adjmCopy <- adjm[,]  # non-bigmem version
   n <- nrow(adjm)
   myRows <- parallel::splitIndices(n,info$nThreads)[[myID+1]]
   mySubmatrix <- adjm[myRows,]

   # find "dead ends," vertices to lead nowhere
   tmp <- rowSums(adjmCopy)
   deadEnds <- which(tmp == 0)

   for (iter in 1:(n-1)) {
      rthreadsBarrier()
      adjmPow[myRows,] <- adjmPow[myRows,] %*% adjmCopy
      for (myRow in myRows) {
         if (done[myRow,1] == 0) {  # this origin vertex myRow not decided yet
            if (adjmPow[myRow,destVertex] > 0) {
               done[myRow,1] <- iter
               done[myRow,2] <- 1
            } else {
               currDests <- which(adjmPow[myRow,] > 0)
               # check subset
               if (length(currDests) > 0)
                  if (intersect(currDests,deadEnds) == currDests)  {
                  done[myRow,1] <- iter
                  done[myRow,2] <- 2
               }
            }
         }
      }
   }

   rthreadsWaitDone()

}
