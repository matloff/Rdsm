# Rthreads

*Threads for R!*  

* Physical shared RAM.

* Parallel computation under the shared-memory paradigm.

* Each thread is a separate instance of R.

* Formerly the **Rdsm** package, but fully rewritten.

# Advantages of Shared-Memory Approach

* Alternative is message-passing, e.g. **parallel** package,
  including via **foreach** interface.

* Faster execution, since there is no expensive repeated passing
  of data from one process to another.

* Threads manage their own task assignment, rather via communication
  from a central process.

* Clearer code.

How it works:

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

# Functions

* **rthreadsSetup(nThreads,IamThread,sharedVars,mutexNames,infoDir**

  Set up central information structure. Create shared variables and mutexes.

* **rthreadsJoin(infoDir)**

  Attach the shared variables and mutexes. Acquire an ID for this thread.
  Wait for the other threads to join.

* **rthreadsMakeSharedVar(varName,nr,nc,infoDir)**, 
  **rthreadsMakeSharedMutex(mutexName,infoDir)**,
  **rthreadsAttachSharedVari(varName,infoDir)**, 
  **rthreadsAttachSharedMutexi(mutexName,infoDir)** 

  As their names imply.

# Example

* Have a number of vectors, each to be sorted.



