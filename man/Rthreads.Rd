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
rthreadsSetup( nThreads, sharedVars = NULL,  mutexNames = NULL, 
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
  \item{...}{Further arguments.}
  \item{object}{Object returned by a qe-series function.}
  \item{newx}{New data to be predicted.}
  \item{newDocs}{Vector of new documents to be predicted.}
  \item{lag}{number of recent values to use in predicting the next.}
  \item{qeName}{Name of qe-series predictive function, e.g. 'qeRF'.}
  \item{stopWords}{Stop lists to use.}
  \item{nTst}{Number of parameter combinations.}
  \item{kTop}{Number of most-frequent words to use.}
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{holdout}{If not NULL, form a holdout set of the specified size.
     After fitting to the remaining data, evaluate accuracy on the test set.}
  \item{qeFtnList}{Character vector of \code{qe*} function names.}
  \item{nReps}{Number of holdout sets to generate.}
  \item{opts}{R list of optional arguments for none, some or all of th
     functions in \code{qeFtnList}, one element per function (NULL is
     all right). See example.}
  \item{seed}{Seed for random number generation.}
  \item{qeftn}{Quoted string, specifying the name of a qe-series
     machine learning method.}
  \item{pars}{R list of hyperparameter ranges.  See 
     \code{regtools::fineTuning}.}
  \item{nCombs}{Number of hyperparameter combinations to run.  
     See \code{regtools::fineTuning}.}
  \item{nXval}{Number of cross-validations to run.  
     See \code{regtools::fineTuning}.}
  \item{showProgress}{If TRUE, show results as they arise.  
     See \code{regtools::fineTuning}.}
}

\details{

Overviews of the functions:

   \itemize{
   
      \item \code{qeTs} is a tool for time series modeling
   
      \item \code{qeText} is a tool for textual modeling
   
      \item \code{qeCompare} facilitates comparison among models
   
      \item \code{qeFT} does a random grid search for optimal hyperparameter
      values
   
   }

}

\examples{

data(mlb1)
# predict Weight in the mlb1 dataset, using qeKNN, with k = 5 and 25,
# with 10 cross-validations
qeFT(mlb1,'Weight','qeKNN',list(k=c(5,25)),nTst=100,nXval=10)

data(svcensus)
qeCompare(svcensus,'wageinc',c('qeLin','qeKNN'),10,list(qeKNN=list(k=100)))
}


\author{
Norm Matloff
}

