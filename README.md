# Rthreads

## *Threads for R!*

* R does not have native threading.

* Fast packages like **data.table** rely on threads at the C++ level,
  using the C/C++ library OpenMP..

* Threaded coding tends to be clearer and faster, compared to
  message-passing.

* Thus having a threads capability in R would greatly enhance
  R's capabilities in parallel processing.

## Implementation

* True physical shared RAM, via **bigmemory** package.

* Parallel computation under the shared-memory paradigm.

* Each thread is a separate instance of R.

* Formerly the **Rdsm** package, but fully rewritten.

# Advantages of Threaded, Shared-Memory Approach

* Alternative is message-passing, e.g. **parallel** package,
  including via **foreach** interface.

* Faster execution, since under the shared-memory approach, there is no
  expensive repeated passing of data from one process to another as in
  message-passing.

* Threads manage their own task assignment, rather via communication
  from a central process.

* Clearer code.

# How It Works

* The shared memory is implemented via the **bigmemory** package.

* The sole data type is matrix. In **bigmemory**, this must be
  explicitly written with two (possibly empty) subscripts,
  e.g. **x[3,2]**, **x[,1:5]**, **x[,]**.

* The related **synchronicity** package provides mutex support.

* Each thread must run in its own terminal window.

  * This is to facilitate debugging application code. 

    Note: Parallel programming is hard, in any form, and thus one may 
    spend much more time debugging code than writing it.

  * Use **tmux** if screen space is an issue. See below.

* Run **rthreadsSetup** in the first window (the "manager
  thread"), then run **rthreadsJoin** in each window.

* Now call your application function code in each window.

# Example 1

We have a number of vectors, each to be sorted.

``` r

# threads configuration: run
# rthreadsSetup(nThreads=2,
#    sharedVars=list(nextRowNum=c(1,1,3),m=c(10,100000000)))

setup <- function()  # run in "manager thread"
{
   # generate vectors to be sorted, of different sizes
   tmp <- c(30000000,70000000)
   set.seed(9999)
   nvals <- sample(tmp,10,replace=TRUE)  # 10 vectors to sort
   for (i in 1:10) {
      n <- nvals[i]
      m[i,1:(n+1)] <- c(n,runif(n))
   }
}

doSorts <- function()  # run in all threads, maybe with system.time()
{

   rowNum <- myID + 1  # my first vector to sort

   while (rowNum <= nrow(m)) {
      # as illustration of parallel operation, see which threads execute
      # sorts on which rows
      print(rowNum)
      n <- m[rowNum,1]
      x <- m[rowNum,2:(n+1)]
      m[rowNum,2:(n+1)] <- sort(x)
      rowNum <- rthreadsAtomicInc('nextRowNum')
   }

   rthreadsWaitDone()

}

```

To run, say with just 2 threads:

1. Open 2 terminal windows, to be referred to as W1 and W2.

2. In W1, run

``` r
rthreadsSetup(nThreads=2,
   sharedVars=list(nextRowNum=c(1,1,3),m=c(10,100000000)))
```

This sets up 2 threads (running in the 2 windows), and 2 shared
variables: **nextRowNum**, a scalar, and **m**, the latter being our
matrix of rows to be sorted, 10 rows of length 100000000 each.

3. In both windows, run

```r 
rthreadsJoin()
```

Here each thread "checks in," attaches the shared variables, sets its
ID, and then waits until all the threads have joined.

4. In W1, run **setup()** to generate the data.

5. In both W1 and W2, run **doSorts()** to do the sorting.

Overview of the code:

* Each thread works on one row of **m** at a time. 

* When a thread finishes sorting a row, it determines the next row to 
  sort by inspecting the shared variable **nextRowNum**. It increments that
  variable by 1, using the old value as the row it will now sort.

* The incrementing much be done *atomically*. Remember, **nextRowNum**
  is a shared variable. Say its value is currently 7, and two threads
  execute the incrementation at about the same time. We'd like one thread
  to next sort row 8 and the other to sort row 9, with the new value of
  **nextRowNum** now being 10. But if there is no constraint on
  simultaneous access, both threads may get the value 7, with
  **nextRowNum** now being 8. Use of **rthreadsAtomicInc** ensures that
  only one thread can access **nextRowNum** at a time.

* Here is the internal code for **rthreadsAtomicInc**:

  ``` r
  function(sharedV,mtx='mutex0',increm=1) 
  {
     mtx <- get(mtx)
     lock(mtx)
     shrdv <- get(sharedV)
     oldVal <- shrdv[1,1]
     newVal <- oldVal + increm
     shrdv[1,1] <- newVal
     unlock(mtx)
     return(oldVal)
  }
  ```
  
  The key here is use of a *mutex* (short for "mutual exclusion"), which
  can be locked and unlocked. While locked, no other thread is allowed to
  enter the given lines of code. If one thread has locked the mutex and
  another thread reaches the **lock** line, it will be blocked until the
  mutex is unlocked. Mutexs come from the **synchronicity** package.

  The internal code for **rthreadsWaitDone** is similar:

  ``` r
  function() 
  {
     rthreadsAtomicInc('nDone')
     while (nDone[1,1] < info$nThreads) {}
  }
  ```

  Again, the **nDone** count must be incremented atomically. In a
  scenario of simultaneous accesss like that above, the threads would wait
  forever.

* Note that non-shared variables have different values in different
  threads.

# Example 2: Shortest paths in a graph

We have a *graph* or *network*, consisting of people, cities or
whatever, with links between some of them, and wish to find the shortest
path from all vertices A to a vertex B. This ia a famous problem, with
lots of applications. Here we take a matrix approach, with a parallel
solution via **Rthreads**.

We treat the case of *directed* graphs, meaning that a link from vertex
i to vertex j does not imply that a link exists in the opposite
direction.  For a graph of v vertices, the *adjacency matrix* M of the
graph is v X v, with the row i, column j element being 1 or 0, depending
on whether there is a link from i to j.

Here is the code:

``` r
# threads configuration: run
#    rthreadsSetup(nThreads=2)

# algorithm assumes a Directed Acyclic Graph (DAG); for test cases, an
# easy 

setup <- function(preDAG,destVertex)  # run in "manager thread"
{
   library(bnlearn)
   # to generate a DAG, take any data frame and run it through, say,
   # bnlearn:hc
   adj <- amat(hc(preDAG))
   n <- nrow(adj)
   rthreadsMakeSharedVar('adjm',n,n,initVal=adj)
   rthreadsMakeSharedVar('adjmPow',n,n,initVal=adj)
   # if in row i = (u,v), u is not 0 then it means this path search ended
   # after iteration u; v = 1 means reached the destination, v = 2
   # means no paths to destination exist
   rthreadsMakeSharedVar('done',n,2,initVal=rep(0,2*n))
   rthreadsMakeSharedVar('imDone',1,1,initVal=0)
   rthreadsMakeSharedVar('NDone',1,1,initVal=0)
   rthreadsMakeSharedVar('dstVrtx',1,1,initVal=destVertex)
   rthreadsInitBarrier()
   return()
}

findMinDists <- function()  
   # run in all threads, maybe with system.time()
{
   if (myID > 0) {
      rthreadsAttachSharedVar('adjm')
      rthreadsAttachSharedVar('adjmPow')
      rthreadsAttachSharedVar('done')
      rthreadsAttachSharedVar('NDone')
      rthreadsAttachSharedVar('dstVrtx')
   } 

   destVertex <- dstVrtx[1,1]

   n <- nrow(adjm[,])
   myRows <- parallel::splitIndices(n,info$nThreads)[[myID+1]]
   mySubmatrix <- adjm[myRows,]

   # find "dead ends," vertices to lead nowhere
   tmp <- rowSums(adjm[,])
   deadEnds <- which(tmp == 0)
   done[deadEnds,1] <- 1
   done[deadEnds,2] <- 2
   # and don't need a path from destVertex to itself
   done[destVertex,] <- c(1,2)
   deadEndsPlusDV <- c(deadEnds,destVertex)

   imDone <- FALSE
   for (iter in 1:(n-1)) {
      rthreadsBarrier()
      if (NDone[1,1] == info$nThreads) return()
      if (iter > 1 && (iter <= n-1))
         adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
      if (!imDone) {
         for (myRow in setdiff(myRows,deadEndsPlusDV)) {
            if (done[myRow,1] == 0) {  # this vertex myRow not decided yet
               if (adjmPow[myRow,destVertex] > 0) {
                  done[myRow,1] <- iter
                  done[myRow,2] <- 1
               } else {
                  currDests <- which(adjmPow[myRow,] > 0)
                  # check subset
                  currDestsEmpty <- (length(currDests) == 0)
                  if (currDestsEmpty ||
                      !currDestsEmpty &&
                         identical(intersect(currDests,deadEnds),currDests))  {
                     done[myRow,1] <- iter
                     done[myRow,2] <- 2
                  }
               }
            }
         }
         if (sum(done[myRows,1] == 0) == 0) {
            imDone <- TRUE
            rthreadsAtomicInc('NDone')
         }
      }
   }

}

```

A key property is that the k-th power of M tells us whether there is a
k-link path from i to j, according to whether the row i, column j
element is nonzero. The matrix powers are computed in parallel, with
each thread being responsible for a subset of rows:

``` r
n <- nrow(adjm[,])
myRows <- parallel::splitIndices(n,info$nThreads)[[myID+1]]
mySubmatrix <- adjm[myRows,]
...
adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
```

As noted, we will find the shortest distance from all vertices A to a
given destination B, which is **destVertex** in the code. We will store
results in the shared matrix **done**, and in fact that object will
contain the final results in the end. If row i in **done** has value
(r,s), it means that in iteration r our search for paths from i to the
destination ended in iteration i. If s = 1, that means the destination
was reached in a path of r links; if s = 2, it is impossible to get from
i to the destination.

We assume the graph is *acyclic*, meaning that once we leave a vertex i,
there is no path back to that vertex. Thus any path can be of length at
most **n-1**, and typically will be shorter. Not only might a path reach
the destination with fewer jumps, but also a path might end at an
*absorbing vertex*, one with no outgoing links. We refer to them as
"dead ends" in the code.

Taking all this into account, we see that even though our **for**
loop has a nominal number of iterations **n-1**, we often will exit the
loop well before that.

Now, let's take a closer look at the beginning of the loop:

``` r
for (iter in 1:(n-1)) {
   rthreadsBarrier()
   if (NDone[1,1] == info$nThreads) return()
   if (iter > 1 && (iter <= n-1))
      adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
```

The *barrier* is an important threads concept. When a thread reaches
that line, it may not proceed further until *all* threads have reached
the line. Why is this needed here? Actually, we don't need it in this
particular case, but I've included it to explain the barrier concept, as
follows.

The concern is that we might have, say, thread 3 starting iteration 10,
but thread 2 is still in iteration 9. When thread 3 modifies the shared
power matrix,

``` r
adjmPow[myRows,] <- adjmPow[myRows,] %*% adjm[,]
```

thread 2 may still be using the old version. If thread 3 modifies the
matrix before thread 2 finishes using the old version, then this thread
will likely compute incorrectly.

That actually can't happen here, as each thread works only with its own
rows of the matrix, not just in the matrix multiplication process, but
also in the path computations, e.g.

``` r
if (adjmPow[myRow,destVertex] > 0) {
   done[myRow,1] <- iter
   done[myRow,2] <- 1
```

But again, in other similar applications, the threads' work is not so
well separated, and a barrier would be needed.

# Facilitating Rthreads Use via 'screen' or 'tmux'

In a given parallel processing project, one may run the same code many
times. With **Rthreads**, this must be done by hand once for each
thread. Thus methods for automating the process would be desirable. The
Unix (Mac or Linux) **screen** and **tmux** utilities can be very
helpful in this regard.

Notably, these utilities enable us to have code running in one window write
a specified string to another window. E.g. say we are in the shell of
Window A. We can do, e.g. 

``` bash
screen -S WindowBScreen
```

in Window B to start a **screen** session there. Then in Window A
we might run

``` bash
screen -S WindowB -X stuff 'ls'$'\n'
```

and the Unix **ls** command will run in Window B just as if we had typed
it there ourself! Moreover, we might be running R in Window A, in which
case we can run the above shell command via R's **system** function

In other words, we can for instance automate the running of
**rthreadsJoin** in all the windows, instead of having to type the
command in each one.

Furthermore, if we are concerned about using up available screen space,
the above utilities allow one to have all the panes of a session in one
physical space on the screen, switching between them on command.

# To Learn More

* [N. Matloff, *Parallel Computing for Data Science
With Examples in R, C++ and CUDA*](https://www.google.com/books/edition/Parallel_Computing_for_Data_Science/SsbECQAAQBAJ?hl=en&gbpv=0)

* [Tutorial on accessing OpenMP via **Rcpp**](https://mfasiolo.github.io/sc2-2019/rcpp_advanced_iii/1_openmp/)
