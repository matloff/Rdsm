
# threads configuration: run
# rthreadsSetup(nThreads=2,
#    sharedVars=list(nextRowNum=c(1,1,3),m=c(10,100000000)))

setup <- function()  # run in "manager thread"
{
   load('../../data/LoansOpenIntro.RData')
   z <- dim(loansOpenIntro)
   nr <- z[1]
   nc <- z[2]
   rthreadsMakeSharedVar('dta',nr,nc,initVal=loansOpenIntro)
   totNumColsPrcoessed[1,1] <- 0
   nextColNum[1,1] <- 1
   rthreadsBarrierInit()
}

doImputation <- function(yName,nToTriggerUpdate)  
{
   yCol <- which(names(dta))
   myColNum <- myID+1  
   myNumColsProcessed <- 0

   myImputes <- list()


   while (colNum <= ncol(dta)) {

      # as illustration of parallel operation, see which threads execute
      # sorts on which rows
      print(colNum)

      # impute this column, if needed
      NAelements <- which(is.na([,dta[,myColNum]))
      if (length(NAelements) > )) {
         lmOut <- lm(dta[,yCol] ~ dta[,-yCol])
         imputes <- predict(lmOut,,dta[,-myColNum])
         myNumColsProcessed <- myNumColsProcessed + 1
         rthreadsAtomicInc('totNumColsPrcoessed',myNumColsProcessed)
         myImputes[[myNumColsProcessed]] <- 
            list(myColNum=myColNum,imputes=imputes,NAelements=NAelements)
      }

      # time to update data?
      if (totNumColsProcessed[1,1] %% nToTriggerUpdate == 0) {
         rthreadsBarrier()
         if (length(myImputes) > 0) 
            for (i in 1:length(myImputes) {
               colIinfo <- myImputes[[i]]
               myColNum <- colIinfo$myColNum
               NAelements <- colIinfo$NAelements
               imputes <- colIinfo$imputes
               dta[NAelements,myColNum] <- imputes
            }
         rthreadsBarrier()
      }



      n <- m[colNum,1]
      x <- m[colNum,2:(n+1)]
      m[colNum,2:(n+1)] <- sort(x)
      colNum <- rthreadsAtomicInc('nextRowNum')
   }

   # rthreadsWaitDone()
   rthreadsBarrier()

}
