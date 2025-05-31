
require(bigmemory)
require(synchronicity)

# sharedVars 

#    excludes mutexes 

#    example value is list(a=c(5,2),b=(2,6)) 

#       specifying shared matrices 'a' and 'b', 
#       the former 5 x 2 and the latter 2 x 6

#    the list values could be more than 2-tuples, having lengths
#    3 or more; the function rthreadsSetup handles the
#    case of length 3
   
utils::globalVariables(c('info','myID','nDone','nJoined'))

rthreadsSetup <- function(
   nThreads,  # number of threads
   sharedVars = NULL,  # see above
   mutexNames = NULL,  # other than 'mutex0'
   infoDir = '~/'
) 
{
   assign('info',
      list(
         nThreads = nThreads,
         infoDir = infoDir,
         sharedVarNames = NULL,
         mutexNames = mutexNames
      ),envir = .GlobalEnv)

   infoFile = paste0(infoDir,'rthreadsInfo.RData')

   rthreadsMakeMutex('mutex0',infoDir='~/')
   rthreadsMakeBarrier()
   rthreadsMakeSharedVar('nJoined',1,1,infoDir='~/',initVal=1)
   rthreadsMakeSharedVar('nDone',1,1,infoDir='~/',initVal=0)

   # set up the shared variables
   if (!is.null(sharedVars)) {
      for (i in 1:length(sharedVars)) {
         varName <- names(sharedVars)[i]
         nrowcoletc <- sharedVars[[i]]
         if (length(nrowcoletc) != 3) {
            rthreadsMakeSharedVar(varName,nrowcoletc[1],nrowcoletc[2],
               infoDir='~/')
         } else {
            rthreadsMakeSharedVar(varName,nrowcoletc[1],nrowcoletc[2],
               infoDir='~/', initVal=nrowcoletc[3])
         }
         info$sharedVarNames <- c(info$sharedVarNames,varName)
      }
   }

   # set up the application-specific mutexes
   mutexNames <- info$mutexNames
   if (!is.null(mutexNames)) {
      for (i in 1:length(info$mutexNames)) {
         mtxname <- info$mutexNames[i]
         tmp <- boost.mutex(mtxname)
         desc <- describe(tmp)
         descFile <- paste0(infoDir,mtxname,'.desc')
         dput(desc,file=descFile)
         info$mutexNames <- c(info$mutexNames,descFile)
      }
   }

   save(info,file=infoFile)
   assign('myID',0,envir = .GlobalEnv)
}

rthreadsJoin <- function(infoDir= '~') 
{

   # check in and get my ID
   infoFile = paste0(infoDir,'/rthreadsInfo.RData')
   load(infoFile)
   assign('info',info,envir = .GlobalEnv); rm(info)
   infoDir <- info$infoDir
   tmp <- get0('myID',envir = .GlobalEnv)
   mgrThread <- !is.null(tmp) && myID == 0
   ### mgrThread <- exists('myID') && myID == 0
   if (!mgrThread) {
      rthreadsAttachSharedVar('nJoined',infoDir='~/')
      rthreadsAttachSharedVar('nDone',infoDir='~/')
      rthreadsAttachMutex('mutex0',infoDir='~/')
      rthreadsAttachSharedVar('barrier0',infoDir='~/')
      rthreadsAttachMutex('barrMutex0',infoDir='~/')
      nj <- rthreadsAtomicInc('nJoined') 
      assign('myID',nj,envir = .GlobalEnv)
   }
   # pick up the shared variables
   sharedVarNames <- info$sharedVarNames
   if (!is.null(sharedVarNames)) {
      for (i in 1:length(sharedVarNames)) {
         rthreadsAttachSharedVar(sharedVarNames[i],infoDir='~/')
      }
   }
   
   # pick up the application-specific mutexes
   mutexNames <- info$mutexNames
   if (!is.null(mutexNames)) {
      for (i in 1:length(mutexNames)) 
         rthreadsAttachSharedMutex(mutexNames[i],infoDir='~/')
   }

   # wait for everyone else
   while (nJoined[1,1] < info$nThreads) {}

}
# atomically increases sharedV by increm, returning old value;
# sharedV is the name of a shared variable; element [1,] is
# incrememted; so, can have vector incrementing vector
rthreadsAtomicInc <- function(sharedV,mtx='mutex0',increm=1) 
{
   mtx <- get(mtx)
   lock(mtx)
   shrdv <- get(sharedV)
   oldVal <- shrdv[1,]
   newVal <- oldVal + increm
   shrdv[1,1] <- newVal
   unlock(mtx)
   return(oldVal)
}

rthreadsMakeBarrier <- function()
{
   rthreadsMakeMutex('barrMutex0')
   get('info',envir = .GlobalEnv)
   rthreadsMakeSharedVar('barrier0',1,2,initVal=c(info$nThreads,0))
}

rthreadsInitBarrier <- function() 
{
   get('info',envir = .GlobalEnv)
   barrier0[1,] <- c(info$nThreads,0)
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
   assign(varName,tmp,envir = .GlobalEnv)
}

# create a mutex shareable across threads
rthreadsMakeMutex <- function(mutexName,infoDir='~/') 
{
   tmp <- boost.mutex()
   desc <- describe(tmp)
   descFile <- paste0(infoDir,mutexName,'.desc')
   dput(desc,file=descFile)
   assign(mutexName,tmp,envir = .GlobalEnv)
}

rthreadsAttachSharedVar <- function(varName,infoDir='~/') 
{
   descFile <- paste0(infoDir,varName,'.desc')
   desc <- dget(descFile)
   assign(varName,attach.big.matrix(desc),envir = .GlobalEnv)
}

rthreadsAttachMutex <- function(mutexName,infoDir='~/') 
{
   descFile <- paste0(infoDir,mutexName,'.desc')
   desc <- dget(descFile)
   assign(mutexName,attach.mutex(desc),envir = .GlobalEnv)
}

rthreadsWaitDone <- function() 
{
   rthreadsAtomicInc('nDone')
   get('info',envir = .GlobalEnv)
   while (nDone[1,1] < info$nThreads) {}
}

rthreadsBarrier <- function() 
{
   mtx <- get('barrMutex0')
   barr <- get('barrier0')
   lock(mtx)
   count <- barr[1,1] - 1
   barr[1,1] <- count
   sense <- barr[1,2]
   if (count == 0) {  # all done
      get('info',envir = .GlobalEnv)
      barr[1,1] <- info$nThreads
      barr[1,2] <- 1 - barr[1,2]
      unlock(mtx)
      return()
   } else {
      unlock(mtx)
      while (barr[1,2] == sense) {}
   }
}

