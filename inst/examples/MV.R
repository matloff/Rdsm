
# threads configuration: run
#    rthreadsSetup(nThreads=2)

# NA imputation, simple use of linear regression, each column's NAs
# replaced by fitted values

# mainly for illustrating barriers; could be made faster in various ways

setup <- function()  # run in "manager thread"
{
   data(NHISlarge)
   nhis.large <- regtools::factorsToDummies(nhis.large,dfOut=FALSE)
   nhis.large <- nhis.large[,-(1:4)]  # omit ID etc.
   # make a small version for testing
   nhis.large <- nhis.large[1:25,]
   print(nhis.large)
   z <- dim(nhis.large)
   nr <- z[1]
   nc <- z[2]
   rthreadsMakeSharedVar('dta',nr,nc,initVal=nhis.large)
   rthreadsInitBarrier()
}

doImputation <- function()  
{
   if (myID > 0) {
      rthreadsAttachSharedVar('dta')
   }
   nc <- ncol(dta)
   nThreads <- info$nThreads

   # in each round, each thread works on one column; they then update
   # the data
   nRounds <- ceiling(nc/nThreads)
   numPerRound <- floor(nc/nRounds)
   for (i in 1:nRounds) {

      myColNum <- (i-1)*numPerRound + myID + 1
   
      # determine which is this thread's column, and 
      # impute this column, if needed
      myImputes <- NULL
      if (myColNum <= nc) {
         print(myColNum)
         NAelements <- which(is.na(dta[,myColNum]))
         if (length(NAelements) > 0) {
            lmOut <- lm(dta[,myColNum] ~ dta[,-myColNum])
            # note: if a row has more than 1 NA, imputed value 
            # will still be NA
            imputes <- lmOut$fitted.values[NAelements]
            myImputes <- 
               list(colNum=myColNum,imputes=imputes,NAelements=NAelements)
         }
      }

      # update data, where needed
      rthreadsBarrier()  # can't change dta while possbily still in use

      if (!is.null(myImputes)) {
         colNum <- myImputes$colNum
         NAelements <- myImputes$NAelements
         imputes <- myImputes$imputes
         dta[NAelements,colNum] <- imputes
      }

      rthreadsBarrier()  # some threads may still be writing to dta

   }

}

