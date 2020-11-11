{ *******************************************************

  Threading utilities for Delphi 2009 and above

  Utilidad para programación concurrente.

  *******************************************************

  2012-2020 Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  ******************************************************* }

unit ThreadUtils.Sync;

{
  SUMMARY:

  - TConditionVariableFix: fix TConditionVariableCS behavior at instance
  destruction

  - TLockCondition and TPrecondition: lock thread execution until
  some condition is met.

  - TSyncCounter: interlocked counter with messaging at a target value

  - TSyncPoint: wait until a number of threads have reached an execution point

  - TRendezvous: pass data to another thread in a synchronized way

  - TThreadSyncHelper: helper class for TThread
}

interface

uses
  SysUtils,
  Classes,
  System.Generics.Collections,
  WinAPI.Windows,
  SyncObjs;

{ TConditionVariableFix

  PURPOUSE:
  To fix wrong behavior of TConditionVariableCS at instance destruction.

  WaitFor:
  Will return true when condition is signaled and false at timeout.
  ESyncObjectException is raised at instance destruction.
}

type
  TConditionVariableFix = class
  private
    destroying: boolean;
    Sync: TConditionVariableCS;
  public
    constructor Create;
    destructor Destroy; override;
    function WaitFor(CriticalSection: TCriticalSection;
      Timeout: LongWord = INFINITE): boolean;
    procedure Release;
    procedure ReleaseAll;
  end;

  { TLockCondition

    PURPOUSE:
    To lock any thread until a certain condition is met.

    GENERAL USAGE:
    - Create a TLockCondition instance
    - enter a critical section
    - Call TLockCondition.WaitFor
    - Leave the critical section

    WAITFOR:
    Call WaitFor to sleep on the given condition and to release the critical
    section specified through the CriticalSection parameter.
    The TimeOut parameter sets the time-out interval.
    When this interval elapses or the condition is met,
    the critical section is reacquired.
    This class ensures that the given condition is allways true after the
    waiting thread has been unlocked and result is true (no need for loops).

    WaitFor will raise ESyncObjectException at instance destruction.

    CriticalSection is reacquired as an atomic operation when the locked
    thread is awaken.

    CONDITIONS:
    Conditions are expressed as references to function returning a
    boolean value. Such function should return "true" when
    the unlocking condition is met.

    SIGNALING STATE CHANGES:
    Any thread that changes the state of the WaitFor condition must call
    TLockCondition.Update.


    EXAMPLE:

    procedure TMyClass.LockUntilCount(limit: integer);
    begin
    CriticalSection.Enter;
    try
    wr := LockCondition.WaitFor(CriticalSection,
    function:boolean
    begin
    result := (self.counter>=limit)
    end;
    finally
    CriticalSection.Leave;
    end;
    end;

    procedure TMyClass.Inc;
    begin
    CriticalSection.Enter;
    inc(self.counter);
    CriticalSection.Leave;
    LockCondition.Update;
    end;
  }

type
  TLockCondition = class
  private
    FSync: TConditionVariableFix;
  public
    constructor Create;
    destructor Destroy; override;
    function WaitFor(CriticalSection: TCriticalSection;
      Condition: TFunc<boolean>; Timeout: cardinal = INFINITE): boolean;
    procedure Update;
  end;

  { TPrecondition

    PURPOUSE:
    To lock any thread until a certain condition is met.

    GENERAL USAGE:
    Same as TLockCondition

    CONDITIONS:
    The locking condition is set at instance creation, so it's result should depend
    on your object's state alone. Whether such condition depends on external
    parameters use TLockCondition instead.

    EXAMPLE:

    constructor TMyClass.Create;
    begin
    ...
    NotEmpty := TPrecondition.Create(
    function: boolean
    begin
    Result := (FBuffer.Count>0);
    end);

    NotFull := TPrecondition.Create(
    function: boolean
    begin
    Result := (FBuffer.Count<FBuffer.Size);
    end);
    ...
    end;

    function TmyClass.Consume: TSomeItem;
    begin
    CriticalSection.Enter;
    NotEmpty.WaitFor(CriticalSection);
    Result := FBuffer.Extract;
    CriticalSection.Leave;
    NotEmpty.Update;
    NotFull.Update;
    end;

    procedure TMyClass.Produce(Item: TSomeItem);
    begin
    CriticalSection.Enter;
    NotFull.WaitFor(CriticalSection);
    FBuffer.Add(Item);
    CriticalSection.Leave;
    NotEmpty.Update;
    NotFull.Update;
    end;
  }

type
  TPrecondition = class(TLockCondition)
  private
    FCondition: TFunc<boolean>;
  public
    constructor Create(Condition: TFunc<boolean>);
    function WaitFor(CriticalSection: TCriticalSection;
      Timeout: cardinal = INFINITE): boolean;
  end;

  { TSyncCounter

    PURPOUSE:
    Synchronized counter which sends messages at a target count

    GENERAL USAGE:
    - Set OnTargetCount and/or OnLeaveTarget event handlers
    - Call Initialize to set a target count and initial value
    - Spawn threads
    - Any thread can add/substract to current count.
    - Every time the counter reaches the target count,
    OnTargetReached is called.
    - Every time the counter changes from the target value to any other value,
    OnLeaveTarget is called.

    NOTES:
    OnTargetReached and OnLeaveTarget are allways called at the main thread.
    Note that current value may have changed at the time
    any of those event handlers is executed.
  }

type
  TSyncCounter = class
  private
    FTargetCount: Integer;
    FCurrentCount: Integer;
    FOnTarget: TNotifyEvent;
    FOnLeave: TNotifyEvent;
  protected
    procedure TestTarget(const currentValue: Integer);
  public
    constructor Create;
    procedure Initialize(const targetValue: Integer;
      const initialValue: Integer);
    procedure Inc(const increment: Integer = 1);
    procedure Dec(const decrement: Integer = 1);
    property OnTargetReached: TNotifyEvent read FOnTarget write FOnTarget;
    property OnLeaveTarget: TNotifyEvent read FOnLeave write FOnLeave;
  end;

  { TSyncPoint

    PURPOUSE:
    Wait until a number of threads have reached certain point of
    execution. Mostly used for waiting termination of
    all spawned threads, without keeping track of their instance identifiers,
    so shared resources are safe to release after that
    (including the TsyncPoint instance itself)

    GENERAL USAGE:
    - Call AddThread every time a new thread is created
    (or set a value at instance creation)
    - Any thread reaching the required execution point should call Reached
    - Call WaitFor to lock until all other threads meet the sync point.

    NOTES:
    Give every thread a chance to reach the sync point, otherwise a deadlock
    may occur.

    EXAMPLE:

    // assume that "sync" is a shared variable
    sync := TSyncPoint.Create(5)
    for i := 1 to 5 do
    MyThread.Create.Start;
    sync.WaitFor;
    <<release shared resources>>
    sync.Free;

    ...

    procedure MyThread.Execute;
    begin
    <<do something>>
    sync.reached;
    end;
  }

type
  TSyncPoint = class
  private
    FThreadCount: Integer;
    Event: TEvent;
    destroying: boolean;
  public
    constructor Create(InitialThreadCount: Integer = 0);
    destructor Destroy; override;
    procedure AddThread;
    procedure Reached;
    function WaitFor(Timeout: cardinal = INFINITE): boolean;
    // procedure MsgWaitFor(Callback: TWaitNotifyEvent);
    property ThreadCount: Integer read FThreadCount;
  end;

  { TRendezvous<T>

    PURPOUSE:
    Pass data to another thread in a synchronized way.

    GENERAL USAGE:
    - one thread calls "send"
    - another thread calls "receive".
    If passed data (of type T) can not be copied by value,
    "receive" should provide a "copy procedure".
    First parameter is the source variable and second parameter is the
    destination variable (also returned by "receive").

    EXAMPLE (Data passed by value):

    R := TRendezvous<integer>.Create;

    // Thread 1
    R.send(89);

    // Thread 2
    intValue := R.receive;

    EXAMPLE (Data passed by reference):

    R := TRendezvous<PByte>.Create;

    // Thread 1
    R.send(buffer);
    buffer[0] := pbyte(8); // this IS SAFE !!!

    // Thread 2
    buffer := R.Receive(
    procedure (source,dest: PByte)
    begin
    CopyMemory(dest,source,BUFFER_SIZE);
    end);

    NOTES:
    - "send" and "receive" are blocking primitives.
    Do not call from the main thread in windows applications
    - "send" and "receive" will unlock pending threads and raise
    an ESyncObjectException after "cancel" is called. Further
    calls will also raise such an exception.

  }

type
  TRendezvous<T> = class
  private
    FData: T;
    sendReady, receiveReady: TEvent;
    sendCS: TCriticalSection;
    receiveCS: TCriticalSection;
    FCancelled: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Cancel;
    function receive(copyProcedure: TProc<T, T>): T; overload;
    function receive: T; overload;
    procedure send(data: T);
    property Cancelled: boolean read FCancelled;
  end;

  {
    TThreadSyncHelper

    PURPOUSE:
    To Introduce new methods to TThread class:

    - WaitForAll(threads,AllOrAny,Timeout)

    Lock calling thread until all threads finish their execution or
    timeout expires.

    - TerminateAll(threads)

    Ask all threads to terminate their execution.

    - StartAll(threads)

    Start all threads
  }

type
  TThreadSyncHelper = class helper for TThread
  protected
    class function APIResult(const value: dword; const length: dword)
      : TWaitResult;
  public
    class function WaitForAll(const threads: TList<TThread>;
      const Timeout: dword = INFINITE): TWaitResult; overload;
    class function WaitForAll(const threads: array of TThread;
      const Timeout: dword = INFINITE): TWaitResult; overload;
    class procedure TerminateAll(const threads: TList<TThread>); overload;
    class procedure TerminateAll(const threads: array of TThread); overload;
    class procedure StartAll(const threads: TList<TThread>); overload;
    class procedure StartAll(const threads: array of TThread); overload;
    class procedure FreeAll(const threads: TList<TThread>); overload;
    class procedure FreeAll(const threads: array of TThread); overload;

  end;

implementation

// -----------------------------------------------------------------------------
// TConditionVariableFix
// -----------------------------------------------------------------------------

constructor TConditionVariableFix.Create;
begin
  Sync := TConditionVariableCS.Create;
  destroying := False;
end;

destructor TConditionVariableFix.Destroy;
begin
  destroying := true;
  Sync.ReleaseAll;
  FreeAndNil(Sync);
  inherited;
end;

function TConditionVariableFix.WaitFor(CriticalSection: TCriticalSection;
  Timeout: LongWord = INFINITE): boolean;
var
  wr: TWaitResult;
begin
  try
    if (destroying) then
      wr := TWaitResult.wrAbandoned
    else
      wr := Sync.WaitFor(CriticalSection, Timeout);
    if (destroying) or (wr = TWaitResult.wrAbandoned) then
      raise EAbort.Create('Abandoned')
    else
      result := (wr = TWaitResult.wrSignaled);
  except
    raise ESyncObjectException.Create
      ('TConditionVariableFix destroyed at WaitFor');
  end;
end;

procedure TConditionVariableFix.Release;
begin
  Sync.Release;
end;

procedure TConditionVariableFix.ReleaseAll;
begin
  Sync.ReleaseAll;
end;

// -----------------------------------------------------------------------------
// TLockCondition
// -----------------------------------------------------------------------------

constructor TLockCondition.Create;
begin
  FSync := TConditionVariableFix.Create;
end;

// -----------------------------------------------------------------------------

destructor TLockCondition.Destroy;
begin
  FSync.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

function TLockCondition.WaitFor(CriticalSection: TCriticalSection;
  Condition: TFunc<boolean>; Timeout: cardinal): boolean;
begin
  if (not Assigned(Condition)) then
    raise ESyncObjectException.Create('TLockCondition: Condition unknown');
  result := true;
  while (result) and (not Condition()) do
    result := FSync.WaitFor(CriticalSection, Timeout);
end;

// -----------------------------------------------------------------------------

procedure TLockCondition.Update;
begin
  FSync.ReleaseAll;
end;

// -----------------------------------------------------------------------------
// TPrecondition
// -----------------------------------------------------------------------------

constructor TPrecondition.Create(Condition: TFunc<System.boolean>);
begin
  if (not Assigned(Condition)) then
    raise ESyncObjectException.Create('TPrecondition: precondition unknown');
  FCondition := Condition;
  inherited Create;
end;

// -----------------------------------------------------------------------------

function TPrecondition.WaitFor(CriticalSection: TCriticalSection;
  Timeout: cardinal): boolean;
begin
  result := inherited WaitFor(CriticalSection, FCondition, Timeout);
end;

// -----------------------------------------------------------------------------
// TSyncCounter
// -----------------------------------------------------------------------------

constructor TSyncCounter.Create;
begin
  FTargetCount := 0;
  FCurrentCount := 0;
  FOnTarget := nil;
  FOnLeave := nil;
end;

// -----------------------------------------------------------------------------

procedure TSyncCounter.TestTarget(const currentValue: Integer);
begin
  if (currentValue = FTargetCount) and Assigned(FOnTarget) then
    TThread.Synchronize(TThread.Current,
      procedure
      begin
        FOnTarget(self);
      end)
  else if (currentValue <> FTargetCount) and Assigned(FOnLeave) then
    TThread.Synchronize(TThread.Current,
      procedure
      begin
        FOnLeave(self);
      end);
end;

// -----------------------------------------------------------------------------

procedure TSyncCounter.Initialize(const targetValue: Integer;
const initialValue: Integer);
begin
  FCurrentCount := initialValue;
  FTargetCount := targetValue;
  TestTarget(initialValue);
end;

// -----------------------------------------------------------------------------

procedure TSyncCounter.Inc(const increment: Integer = 1);
begin
  TestTarget(TInterlocked.Add(FCurrentCount, increment));
end;

// -----------------------------------------------------------------------------

procedure TSyncCounter.Dec(const decrement: Integer = 1);
begin
  TestTarget(TInterlocked.Add(FCurrentCount, (decrement * -1)));
end;

// -----------------------------------------------------------------------------
// TSyncPoint
// -----------------------------------------------------------------------------

constructor TSyncPoint.Create(InitialThreadCount: Integer);
begin
  destroying := False;
  if (InitialThreadCount < 0) then
    InitialThreadCount := 0;
  FThreadCount := InitialThreadCount;
  Event := TEvent.Create(nil, true, (InitialThreadCount = 0), '');
end;

// -----------------------------------------------------------------------------

destructor TSyncPoint.Destroy;
begin
  destroying := true;
  Event.SetEvent; // unlock any waiting thread
  FreeAndNil(Event);
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TSyncPoint.AddThread;
begin
  if (InterlockedIncrement(FThreadCount) = 0) then
    Event.SetEvent
  else
    Event.ResetEvent;
end;

// -----------------------------------------------------------------------------

procedure TSyncPoint.Reached;
begin
  if (InterlockedDecrement(FThreadCount) = 0) then
    Event.SetEvent;
end;

// -----------------------------------------------------------------------------

function TSyncPoint.WaitFor(Timeout: cardinal): boolean;
begin
  try
    result := (Event.WaitFor(Timeout) = TWaitResult.wrSignaled);
    if (destroying) then
      raise EAbort.Create('Abandoned');
  except
    raise ESyncObjectException.Create('TSyncPoint destroyed at WaitFor');
  end;
end;

// -----------------------------------------------------------------------------
// TRendezvous<T>
// -----------------------------------------------------------------------------

constructor TRendezvous<T>.Create;
begin
  sendReady := TEvent.Create(nil, False, False, '');
  receiveReady := TEvent.Create(nil, False, False, '');
  sendCS := TCriticalSection.Create;
  receiveCS := TCriticalSection.Create;
  FCancelled := False;
end;

// -----------------------------------------------------------------------------

destructor TRendezvous<T>.Destroy;
begin
  sendReady.Free;
  receiveReady.Free;
  sendCS.Free;
  receiveCS.Free;
end;

// -----------------------------------------------------------------------------

procedure TRendezvous<T>.send(data: T);
begin
  sendCS.Enter;
  try
    if (FCancelled) then
      raise ESyncObjectException.Create('Sync object was cancelled');
    FData := data;
    sendReady.SetEvent;
    receiveReady.WaitFor;
  finally
    sendCS.Leave;
  end;
  if (FCancelled) then
    raise ESyncObjectException.Create('Sync object was cancelled');
end;

// -----------------------------------------------------------------------------

function TRendezvous<T>.receive: T;
begin
  receiveCS.Enter;
  try
    if (FCancelled) then
      raise ESyncObjectException.Create('Sync object was cancelled');
    sendReady.WaitFor;
    if (not FCancelled) then
    begin
      result := FData;
      receiveReady.SetEvent;
    end;
  finally
    receiveCS.Leave;
  end;
  if (FCancelled) then
    raise ESyncObjectException.Create('Sync object was cancelled');
end;

// -----------------------------------------------------------------------------

function TRendezvous<T>.receive(copyProcedure: TProc<T, T>): T;
begin
  receiveCS.Enter;
  try
    if (FCancelled) then
      raise ESyncObjectException.Create('Sync object was cancelled');
    sendReady.WaitFor;
    if (not FCancelled) then
    begin
      result := FData;
      if Assigned(copyProcedure) then
        try
          copyProcedure(FData, result);
        except
        end;
      receiveReady.SetEvent;
    end;
  finally
    receiveCS.Leave;
  end;
  if (FCancelled) then
    raise ESyncObjectException.Create('Sync object was cancelled');
end;

// -----------------------------------------------------------------------------

procedure TRendezvous<T>.Cancel;
begin
  FCancelled := true;
  receiveReady.SetEvent;
  sendReady.SetEvent;
end;

// -----------------------------------------------------------------------------
// TThreadSyncHelper
// -----------------------------------------------------------------------------

// auxiliary

class function TThreadSyncHelper.APIResult(const value: dword;
const length: dword): TWaitResult;
begin
  if (value < (WAIT_OBJECT_0 + length)) then
    result := TWaitResult.wrSignaled
  else if (value >= WAIT_ABANDONED_0) and (value < (WAIT_ABANDONED_0 + length))
  then
    result := TWaitResult.wrAbandoned
  else if value = WAIT_TIMEOUT then
    result := TWaitResult.wrTimeout
  else
    result := TWaitResult.wrError;
end;

// -----------------------------------------------------------------------------

class function TThreadSyncHelper.WaitForAll(const threads: TList<TThread>;
const Timeout: dword = INFINITE): TWaitResult;
var
  handles: array of THandle;
  i: Integer;
begin
  if (threads <> nil) and (threads.Count > 0) then
  begin
    SetLength(handles, threads.Count);
    for i := 0 to threads.Count - 1 do
      handles[i] := threads.Items[i].Handle;
    i := WaitForMultipleObjects(length(handles), @handles[0], true, Timeout);
    result := APIResult(i, length(handles));
  end
  else
    result := TWaitResult.wrError;
end;

// -----------------------------------------------------------------------------

class function TThreadSyncHelper.WaitForAll(const threads: array of TThread;
const Timeout: dword = INFINITE): TWaitResult;
var
  handles: array of THandle;
  i: Integer;
begin
  SetLength(handles, length(threads));
  for i := 0 to length(threads) - 1 do
    handles[i] := threads[i].Handle;
  i := WaitForMultipleObjects(length(handles), @handles[0], true, Timeout);
  result := APIResult(i, length(handles));
end;

// -----------------------------------------------------------------------------

class procedure TThreadSyncHelper.TerminateAll(const threads: TList<TThread>);
var
  i: Integer;
begin
  if (threads <> nil) then
    for i := 0 to threads.Count - 1 do
      threads.Items[i].Terminate;
end;

// -----------------------------------------------------------------------------

class procedure TThreadSyncHelper.TerminateAll(const threads: array of TThread);
var
  i: Integer;
begin
  for i := Low(threads) to high(threads) do
    threads[i].Terminate;
end;

// -----------------------------------------------------------------------------

class procedure TThreadSyncHelper.StartAll(const threads: TList<TThread>);
var
  i: Integer;
begin
  if (threads <> nil) then
    for i := 0 to threads.Count - 1 do
      threads.Items[i].Start;
end;

// -----------------------------------------------------------------------------

class procedure TThreadSyncHelper.StartAll(const threads: array of TThread);
var
  i: Integer;
begin
  for i := Low(threads) to high(threads) do
    threads[i].Start;
end;

// -----------------------------------------------------------------------------

class procedure TThreadSyncHelper.FreeAll(const threads: TList<TThread>);
var
  i: Integer;
begin
  if (threads <> nil) then
    for i := 0 to threads.Count - 1 do
      threads.Items[i].Free;
end;

// -----------------------------------------------------------------------------

class procedure TThreadSyncHelper.FreeAll(const threads: array of TThread);
var
  i: Integer;
begin
  for i := Low(threads) to high(threads) do
    threads[i].Free;
end;

// -----------------------------------------------------------------------------

end.
