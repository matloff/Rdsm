# Rthreads

*Threads for R!*  

* Physical shared RAM.

* Parallel computation under the shared-memory paradigm.

* Each thread is a separate instance of R.

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

* One runs **rthreadsSetup** in the first window, then **rthreadsJoin**
  in each window.

* Now call your application function code in each window.

Functions:

* **rthreadsSetup(nThreads,IamThread,codeSource,codeCall,
  sharedVars,mutexNames,infoDir**

  Set up central information structure. Create shared variables and mutexes.
  Specify where the application code is, and how to call it.

* **rthreadsJoin(infoDir)**

  Attach the shared variables and mutexes. Acquire an ID for this thread.
  Wait for the other threads to join.

* **rthreadsMakeSharedVar(varName,nr,nc,infoDir)**, 
  **rthreadsMakeSharedMutex(mutexName,infoDir)**,
  **rthreadsAttachSharedVari(varName,infoDir)**, 
  **rthreadsAttachSharedMutexi(mutexName,infoDir)** 

  As their names imply.

