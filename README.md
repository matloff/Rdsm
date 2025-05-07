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

  * Use **tmux** if screen space is an issue. See below.

* Run **rthreadsSetup** in the first window (the "manager
  thread"), then run **rthreadsJoin** in each window.

* Now call your application function code in each window.

# Example

Have a number of vectors, each to be sorted.

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

   rowNum <- myID  # my first vector to sort

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

3. In W1 run

```r 
rthreadsJoin(mgrThread=TRUE)
```

and in W2 run

```r 
rthreadsJoin(mgrThread=FALSE)
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
