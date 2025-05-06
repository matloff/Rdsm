# Rthreads

## *Threads for R!*

* R does not have native threading.

* Fast packages like **data.table** rely on threads at the C++ level,
  using the C/C++ library OpenMP..

* Threaded coding tends to be clearer and faster.

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

  * Use **tmux** if screen space is an issue.

  * Future editions will automate the window creation and run
    of **rthreadsJoin**.

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

This sets up 2 thraads (running in the 2 windows), and 2 shared
variables **nextRowNum**, a scalar, and **m**, our matrix of rows to be
sorted, 10 rows of length 100000000 each.

3. In W1 run

```r 
rthreadsJoin(mgrThread=TRUE)
```

and in W2 run

```r 
rthreadsJoin(mgrThread=FALSE)
```

Here each thread "checks in," and attaches the shared variables.

4. In W1, run **setup()** to generate the data.

5. In both W1 and W2, run **doSorts()** to do the sorting.

Overview of the code:

* Each thread works on one row of **m** at a time. 

* When a thread finishes sorting a row, it determines the next row to 
  sort by the shared variable **nextRowNum**. It increments that
  variable by 1, using the new value as the row it will now sort.

* The incrementing much be done *atomically*. Remember, **nextRowNum**
  is a shared variable. Say its value is currently 7, and two threads
  execute the incrementation at about the same time. We'd like one
  thread to next sort row 8 and the other to sort row 9, with the
  new value of **nextRowNum** now being 10. But if there is no
  constraint, both threads may get the value 7, with **nextRowNum**
  now being 8. Use of **rthreadsAtomicInc** ensures that only one thread
  can access **nextRowNum** at a time.

* Note that non-shared variables have different values in different
  threads.

# To Learn More

* [N. Matloff, *Parallel Computing for Data Science
With Examples in R, C++ and CUDA*](https://www.google.com/books/edition/Parallel_Computing_for_Data_Science/SsbECQAAQBAJ?hl=en&gbpv=0)

* [Tutorial on accessing OpenMP via **Rcpp**](https://mfasiolo.github.io/sc2-2019/rcpp_advanced_iii/1_openmp/)
