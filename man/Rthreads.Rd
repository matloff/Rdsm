\name{Rthreads}
\alias{rthreadsSetup}
\alias{rthreadsAtomicInc}
\alias{rthreadsAttachSharedVar}
\alias{rthreadsInitBarrier}
\alias{rthreadsMakeBarrier}
\alias{rthreadsMakeSharedVariable}
\alias{rthreadsWaitDone}
\alias{rthreadsAttachMutex}
\alias{rthreadsBarrier}
\alias{rthreadsJoin}
\alias{rthreadsMakeMutex}

\title{Rthreads}

\description{
Utilties to implement threads programming in R.
}

\usage{
rthreadsSetup(nThreads, sharedVars = NULL,  mutexNames = NULL, 
   infoDir = '~/')
rthreadsAtomicInc(sharedV,mtx='mutex0',increm=1)
rthreadsAttachSharedVar(varName,infoDir='~/')
rthreadsInitBarrier()
rthreadsMakeBarrier()
rthreadsMakeSharedVariable(varName,nr,nc,infoDir='~/',initVal=NULL)
rthreadsWaitDone()
rthreadsAttachMutex(mutexName,infoDir='~/')
rthreadsBarrier()
rthreadsJoin(infoDir= '~')
rthreadsMakeMutex(mutexName,infoDir='~/')
}

\arguments{
  \item{nThreads}{Number of threads.}
  \item{sharedVars}{Quoted names.}
  \item{mutexNames}{Quoted names.}
  \item{infoDir}{Directory containing the shared objects.}
  \item{sharedV}{Quoted name.}
  \item{mtx}{Quoted name.}
  \item{increm}{Amount to add to shared variable. Can be a vector.}
  \item{varName}{Quoted name.}
  \item{nr}{Number of rows.}
  \item{nc}{Number of columns.}
  \item{mutexName}{Quoted name.}
  \item{initVal}{Initial value of shared variable.}
}

\details{

One sets up terminal windows, one for each thread. In the first window,
one calls \code{rthreadsSetup}, then \code{rthreadsJoin} in each window.

Shared variables must be of matrix type (a restriction of
\code{bigmemory}). One creates them via \code{rthreadsMakeSharedVariable}
in one window (optionally in \code{rthreadsSetup}), then calls
\code{rthreadsAttachSharedVar} in the other windows to share them.

See vignette for more details.

}

\author{
Norm Matloff
}

