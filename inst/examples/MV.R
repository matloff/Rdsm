
# threads configuration: run
# rthreadsSetup(nThreads=2)

setup <- function()  # run in "manager thread"
{
   load('../../data/LoansOpenIntro.RData')
   loansOpenIntro <- factorsToDummies(loansOpenIntro,dfOut=FALSE)
   loansOpenIntro <- loansOpenIntro[1:100,1:25]
   z <- dim(loansOpenIntro)
   nr <- z[1]
   nc <- z[2]
   rthreadsMakeSharedVar('dta',nr,nc,initVal=loansOpenIntro)
   rthreadsMakeSharedVar('totNumColsProcessed',1,1,initVal=0)
   rthreadsMakeSharedVar('nextColNum',1,1,initVal=1)
   rthreadsInitBarrier()
}

doImputation <- function(nToTriggerUpdate)  
{
   myColNum <- myID+1  
   myNumColsProcessed <- 0
   myImputes <- list()
   if (myID > 0) {
      rthreadsAttachSharedVar('dta')
      rthreadsAttachSharedVar('totNumColsProcessed')
      rthreadsAttachSharedVar('nextColNum')
   }

   while (myColNum <= ncol(dta)) {

      # as illustration of parallel operation, see which threads impute
      # which columns
      print(myColNum)

      # impute this column, if needed
      NAelements <- which(is.na(dta[,myColNum]))
      if (length(NAelements) > 0) {
         lmOut <- lm(dta[,myColNum] ~ dta[,-myColNum])
         imputes <- lmOut$fitted.values
         myNumColsProcessed <- myNumColsProcessed + 1
         rthreadsAtomicInc('totNumColsProcessed')
         myImputes[[myNumColsProcessed]] <- 
            list(myColNum=myColNum,imputes=imputes,NAelements=NAelements)
         # note that this may leave "holes" in myImputes
      }

      # time to update data?
      if (totNumColsProcessed[1,1] > 0 &&
          totNumColsProcessed[1,1] %% nToTriggerUpdate == 0) {
         rthreadsBarrier()  # can't change dta while possbily still in use
         if (length(myImputes) > 0) {
            for (i in 1:length(myImputes)) {
               colIinfo <- myImputes[[i]]
               if (!is.null(colIinfo)) {
                  myColNum <- colIinfo$myColNum
                  NAelements <- colIinfo$NAelements
                  imputes <- colIinfo$imputes
                  dta[NAelements,myColNum] <- imputes
               }
            }
            myImputes <- list()
         }
         rthreadsBarrier()  # some threads may still be writing to dta
      }

      myColNum <- rthreadsAtomicInc('nextColNum')
   }

   rthreadsBarrier()

}
