\name{Rthreads}
\alias{rthreadsSetup}
\alias{rthreadsAtomicInc}
\alias{rthreadsInitBarrier}
\alias{rthreadsMakeBarrier}
\alias{rthreadsMakeSharedVar}
\alias{rthreadsAttachSharedVar}
\alias{rthreadsWaitDone}
\alias{rthreadsAttachMutex}
\alias{rthreadsBarrier}
\alias{rthreadsJoin}
\alias{rthreadsMakeMutex}
\alias{quickstart}

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
rthreadsMakeSharedVar(varName,nr,nc,infoDir='~/',initVal=NULL)
rthreadsWaitDone()
rthreadsAttachMutex(mutexName,infoDir='~/')
rthreadsBarrier()
rthreadsJoin(infoDir= '~')
rthreadsMakeMutex(mutexName,infoDir='~/')
quickstart()
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

Threaded programming is inherently based on the concept of sharing
information via global variables. In this package, the globals are
stored in the R environments \code{sharedGlobals} and
\code{myGlobals}.

One sets up terminal windows, one for each thread. In the first window,
one calls \code{rthreadsSetup}, then \code{rthreadsJoin} in each window.

Shared variables must be of matrix type (a restriction of
\code{bigmemory}). One creates them via \code{rthreadsMakeSharedVariable}
in one window (optionally in \code{rthreadsSetup}), then calls
\code{rthreadsAttachSharedVar} in the other windows to share them.

Type 'quickstart()' for detailed examples.

}

\value{

Almost all functions have no return value, operating via side effects to
the global variables.. The exception is \code{rthreadsAtomic}, which
returns the old value of the incremented variable.

}

\author{
Norm Matloff
}

