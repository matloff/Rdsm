
require(bigmemory)
require(synchronicity)

# note on sharedVars: excludes mutexes but includes barriers;
# example value is list(a=c(5,2),b=(2,6))

rthreadsSetup <- function(
   nThreads,  # number of threads
   IamThread = TRUE,
   sharedVars,  # see above
   mutexNames = NULL,  # other than 'mutex0'
   infoDir = '~/'
) 
{

   info <<- list(
      nThreads = nThreads,
      infoDir <- infoDir,
      sharedVarNames = NULL,
      mutexNames = mutexNames
   )

   infoFile = paste0(infoDir,'rthreadsInfo.RData')

   # setup mutex0 and nJoined
   rthreadsMakeSharedMutex('mutex0',infoDir)
   rthreadsMakeSharedVar('nJoined',1,1,infoDir)
   nJoined[1,1] <- as.numeric(IamThread)
   if (IamThread) info$myID <- 1

   # set up the shared variables
   for (i in 1:length(sharedVars)) {
      varName <- names(sharedVars)[i]
      nrowcol <- sharedVars[[i]]
      rthreadsMakeSharedVar(varName,nrowcol[1],nrowcol[2],infoDir)
      info$sharedVarNames <- c(info$sharedVarNames,varName)
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

}

rthreadsJoin <- function(infoDir= '~') 
{

   # check in and get my ID
   infoFile = paste0(infoDir,'/rthreadsInfo.RData')
   load(infoFile)
   info <<- info; rm(info)
   infoDir <- info$infoDir
   rthreadsAttachSharedVar('nJoined',infoDir)
   rthreadsAttachSharedMutex('mutex0',infoDir)
   lock(mutex0)
   oldnj <- nJoined[1,1]
   nj <- oldnj + 1
   nJoined[1,1] <- nj
   info$myID <- nj
   unlock(mutex0)
   # pick up the shared variables
   sharedVarNames <- info$sharedVarNames
   for (i in 1:length(sharedVarNames)) {
      rthreadsAttachSharedVar(sharedVarNames[i],infoDir)
   }
   # pick up the application-specific mutexes
   mutexNames <- info$mutexNames
   if (!is.null(mutexNames)) {
      for (i in 1:length(mutexNames)) 
         rthreadsAttachSharedMutex(mutexNames[i],infoDir)
   }

   # wait for everyone else
   while (nJoined[1,1] < info$nThreads) {};

}

# create a variable shareable across threads
rthreadsMakeSharedVar <- function(varName,nr,nc,infoDir) 
{
   tmp <- big.matrix(nr,nc,type='double')
   desc <- describe(tmp)
   descFile <- paste0(infoDir,varName,'.desc')
   dput(desc,file=descFile)
   assign(varName,tmp,envir = .GlobalEnv)
}

# create a mutex shareable across threads
rthreadsMakeSharedMutex <- function(mutexName,infoDir) 
{
   tmp <- boost.mutex()
   desc <- describe(tmp)
   descFile <- paste0(infoDir,mutexName,'.desc')
   dput(desc,file=descFile)
   assign(mutexName,tmp,envir = .GlobalEnv)
}

rthreadsAttachSharedVar <- function(varName,infoDir) 
{
   descFile <- paste0(infoDir,varName,'.desc')
   desc <- dget(descFile)
   assign(varName,attach.big.matrix(desc),envir = .GlobalEnv)
}

rthreadsAttachSharedMutex <- function(mutexName,infoDir) 
{
   descFile <- paste0(infoDir,mutexName,'.desc')
   desc <- dget(descFile)
   assign(mutexName,attach.mutex(desc),envir = .GlobalEnv)
}

