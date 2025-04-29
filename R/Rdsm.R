

# note on sharedVars: excludes mutexes but includes barriers;
# example value is list(a=c(5,2),b=(2,6))

rdsmSetup <- function(
   nThreads,  # number of threads
   IamThread = TRUE,
   nJoined = as.numeric(IamThread),
   codeSource = '~/mycode.R',
   codeCall,  # best in the form of do.call()
   sharedVars,  # see above
   mutexNames = NULL,  # other than 'mutex0'
   barriers,
   infoDir = '~/',
   infoFile = pastep(infoDir,'rdsmInfo.RData')
) 
{

   info <- list(
      nThreads = nThreads,
      codeSource = codeSource,
      codeCall = codeCall,
      sharedVarDescFiles = NULL,
      mutexNames = mutexNames
   )

   # set up the shared variables
   for (i in 1:length(sharedVars)) {
      varName <- names(sharedVars)[i]
      nrowcol <- sharedVars[[i]]
      assign(varName,big.matrix(nrowcol[1],nrowcol[2],type='double'))
      desc <- describe(get(varName))
      descFile <- paste0(infoDir,varName,'.desc')
      dput(desc,file=descFile)
      info$sharedVarDescFiles <- c(info$sharedVarDescFiles,descFile)
   }

   # set up the mutexes, first the required one
   mtx0 <- boost.mutex('mtx0')
   desc <- describe(mtx0)
   dput(desc,file=descFile)

   for (i in 1:length(info$mutexNames)) {
      mtxname <- info$mutexNames[i]
      tmp <- boost.mutex(mtxname)
      desc <- describe(get(mtxname))
      descFile <- paste0(infoDir,mtxname,'.desc')
      dput(desc,file=descFile)
      info$mutexNames <- c(info$mutexNames,descFile)

   }

   save(info,infoFile)

}

rsdmJoin <- function(infoDir= '~') 
{

   # check in and get my ID
   infoFile = pastep(infoDir,'rdsmInfo.RData')
   load(infoFile)



}
