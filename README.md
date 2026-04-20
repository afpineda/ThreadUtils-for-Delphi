# Threading utilities for Delphi

This library was conceived nearly one decade before publishing.
At the time, 
Delphi lacked high-level thread-safe data types and synchronisation primitives, 
and had no specific API for processing Windows messages in background.

Its main motivation is the difficulty in achieving a smooth termination 
of multi-threaded Windows applications (event-driven).

## Threading utilities

> File: `ThreadUtils.pas`

- TCustomThread<ArgType>:

  Simplifies the writing of threaded code, avoiding the need to derive a new
  class. Threaded code is written as an event handler or as a reference to
  procedure.

- TTaskHelper:

  Adds a simplified TTask.Run method which can run both procedures
  and functions. No need for IFuture on event-driven apps.

## Thread syncrhonization utilities

> File: `ThreadUtils.Sync.pas`

- TConditionVariableFix:

  Mimics `TConditionVariable` but it raises `ESyncObjectException`
  in `WaitFor()` when the instance is destroyed while waiting.
  This is the base for all other synchronization primitives in the library,
  which also behave in this way.

- TLockCondition:

  Blocks the caller thread until some condition is met
  (no need for loops), or a timeout expires,
  and reacquires a critical section as an atomic operation.

- TPrecondition:

  It works as `TLockCondition` but the locking condition depends
  on the owner object's state alone.

- TSyncCounter:

  An atomic integer counter which runs a callback in the main thread
  when a certain count is reached or leaved.
  This was conceived as a way to keep track of pending background work
  and update the GUI accordingly.

- TSyncPoint:

  Enables a "master" thread to wait for all "slave" threads to
  reach a certain execution point.
  It was conceived as a way to achieve gracefull program termination
  by waiting for all workers to finish before releasing resources.

- TRendezvous<T>

  Enables a thread to send data to another thread in a synchronized
  way. It works as "copy-by-reference" or "copy-by-value" parameter passing.
  It was conceived as a way to avoid unsafe shared variables.

- TThreadSyncHelper

  A helper class to start, stop, free and wait for termination
  of a collection of threads instead of one by one.
  
## Thread-safe data types

> File: `ThreadUtils.SafeDataTypes.pas`

- TThreadSafeQueue<T>:

  A thread-safe queue.
  The `Enqueue()` primitive is non-blocking.

- TThreadSafeStack<T>

  A thread-safe stack.

  
  
