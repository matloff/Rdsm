
require(bigmemory)
require(synchronicity)

rthreadsSetup <- function(
   nThreads,  # number of threads
   sharedVars = NULL,  # see above
   mutexNames = NULL,  # other than 'mutex0'
   infoDir = '~/'
) 
{

   info <- list(
              nThreads = nThreads,
              infoDir = infoDir,
              sharedVarNames = names(sharedVars),
              mutexNames = mutexNames
           )

   infoFile = paste0(infoDir,'rthreadsInfo.RData')
   save(info,file=infoFile)

   # set up myGlobals
   tmp <- new.env()
   assign('myGlobals',tmp,envir = .GlobalEnv)
   myGlobals$myID <- 0
   myGlobals$info <- info

   # set up sharedGlobals
   tmp <- new.env()
   assign('sharedGlobals',tmp,envir = .GlobalEnv)

   rthreadsMakeMutex('mutex0',infoDir='~/')
   rthreadsMakeBarrier()
   rthreadsMakeSharedVar('nJoined',1,1,infoDir='~/',initVal=1)
   rthreadsMakeSharedVar('nDone',1,1,infoDir='~/',initVal=0)

   # set up the shared variables
   if (!is.null(sharedVars)) {
      for (i in 1:length(sharedVars)) {
         varName <- names(sharedVars)[i]
         nrowcoletc <- sharedVars[[i]]
         if (length(nrowcoletc) == 2) {
            rthreadsMakeSharedVar(varName,nrowcoletc[1],nrowcoletc[2],
               infoDir='~/')
         } else {
            rthreadsMakeSharedVar(varName,nrowcoletc[1],nrowcoletc[2],
               infoDir='~/', initVal=nrowcoletc[3])
         }
         myGlobals$info$sharedVarNames <- 
            c(myGlobals$info$sharedVarNames,varName)
      }
   }

   # set up the application-specific mutexes
   mutexNames <- myGlobals$info$mutexNames
   if (!is.null(mutexNames)) {
      for (i in 1:length(myGlobals$info$mutexNames)) {
         mtxname <- myGlobals$info$mutexNames[i]
         tmp <- boost.mutex(mtxname)
         desc <- describe(tmp)
         descFile <- paste0(infoDir,mtxname,'.desc')
         dput(desc,file=descFile)
         myGlobals$info$mutexNames <- c(myGlobals$info$mutexNames,descFile)
      }
   }
   
}

rthreadsJoin <- function(infoDir= '~') 
{

   # check in and get my ID
   info <- NULL  # so CRAN won't object
   infoFile = paste0(infoDir,'/rthreadsInfo.RData')
   load(infoFile)
   tmp <- get0('myGlobals',envir = .GlobalEnv)
   mgrThread <- !is.null(tmp) && myGlobals$myID == 0
   if (!mgrThread) {
      tmp <- new.env()
      assign('sharedGlobals',tmp,envir = .GlobalEnv)
      rthreadsAttachSharedVar('nJoined',infoDir='~/')
      rthreadsAttachMutex('mutex0',infoDir='~/')
      nj <- rthreadsAtomicInc('nJoined') 
      tmp <- new.env()
      assign('myGlobals',tmp,envir = .GlobalEnv)
      myGlobals$myID <- nj
      myGlobals$info <- info
      rthreadsAttachSharedVar('nDone',infoDir='~/')
      rthreadsAttachSharedVar('barrier0',infoDir='~/')
      rthreadsAttachMutex('barrMutex0',infoDir='~/')
      # pick up the shared variables
      sharedVarNames <- myGlobals$info$sharedVarNames
      if (!is.null(sharedVarNames)) {
         for (i in 1:length(sharedVarNames)) {
            rthreadsAttachSharedVar(sharedVarNames[i],infoDir='~/')
         }
      }
      # pick up the application-specific mutexes
      mutexNames <- myGlobals$info$mutexNames
      if (!is.null(mutexNames)) {
         for (i in 1:length(mutexNames)) 
            rthreadsAttachMutex(mutexNames[i],infoDir='~/')
      }
   }

   # wait for everyone else
   while (sharedGlobals$nJoined[1,1] < myGlobals$info$nThreads) {}

}

# atomically increases sharedV by increm, returning old value; sharedV
# is the name of a shared variable (NOT prefixed by 'sharedGlobals$');
# element [1,] is incremented; so, can have vector incrementing vector
rthreadsAtomicInc <- function(sharedV,mtx='mutex0',increm=1) 
{
   mtx <- get(mtx,envir=sharedGlobals)
   synchronicity::lock(mtx)
   shrdv <- get(sharedV,envir=sharedGlobals)
   oldVal <- shrdv[1,]
   newVal <- oldVal + increm
   shrdv[1,1] <- newVal
   synchronicity::unlock(mtx)
   return(oldVal)
}

rthreadsMakeBarrier <- function()
{
   rthreadsMakeMutex('barrMutex0')
   rthreadsMakeSharedVar('barrier0',1,2,initVal=c(myGlobals$info$nThreads,0))
}

rthreadsInitBarrier <- function() 
{
   sharedGlobals$barrier0[1,] <- c(myGlobals$info$nThreads,0)
}

# create a variable shareable across threads
rthreadsMakeSharedVar <- function(varName,nr,nc,infoDir='~/',initVal=NULL) 
{
   tmp <- big.matrix(nr,nc,type='double')
   if (!is.null(initVal)) {
      tmp[,] <- initVal
   }
   desc <- describe(tmp)
   descFile <- paste0(infoDir,varName,'.desc')
   dput(desc,file=descFile)
   sharedGlobals[[varName]] <- tmp
}

# create a mutex shareable across threads
rthreadsMakeMutex <- function(mutexName,infoDir='~/') 
{
   tmp <- boost.mutex()
   desc <- describe(tmp)
   descFile <- paste0(infoDir,mutexName,'.desc')
   dput(desc,file=descFile)
   sharedGlobals[[mutexName]] <- tmp
}

rthreadsAttachSharedVar <- function(varName,infoDir='~/') 
{
   descFile <- paste0(infoDir,varName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[varName]] <- attach.big.matrix(desc)
}

rthreadsAttachMutex <- function(mutexName,infoDir='~/') 
{
   descFile <- paste0(infoDir,mutexName,'.desc')
   desc <- dget(descFile)
   sharedGlobals[[mutexName]] <- attach.mutex(desc)
}

rthreadsWaitDone <- function() 
{
   rthreadsAtomicInc('nDone')
   while (sharedGlobals$nDone[1,1] < myGlobals$info$nThreads) {}
}

rthreadsBarrier <- function() 
{
   mtx <- get('barrMutex0',envir=sharedGlobals)
   barr <- get('barrier0',envir=sharedGlobals)
   synchronicity::lock(mtx)
   count <- barr[1,1] - 1
   barr[1,1] <- count
   sense <- barr[1,2]
   if (count == 0) {  # all done
      barr[1,1] <- myGlobals$info$nThreads
      barr[1,2] <- 1 - barr[1,2]
      synchronicity::unlock(mtx)
      return()
   } else {
      synchronicity::unlock(mtx)
      while (barr[1,2] == sense) {}
   }
}

quickstart <- function() 
{
   browseURL(system.file("doc", "QuickStart.pdf", package = "Rthreads"))
}
