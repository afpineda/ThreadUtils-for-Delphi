{ *******************************************************

  Threading utilities for Delphi 2009 and above

  Utilidad para programación concurrente.

  *******************************************************

  2020 Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  ******************************************************* }

unit ThreadUtils.SafeDataTypes;

{
  SUMMARY:

  - TThreadSafeQueue: Thread-safe implementation of first-in first-out queue.

  - TThreadSafeStack: Thread-safe implementation of last-in first-out stack.
}

interface

uses
  System.SyncObjs, ThreadUtils.Sync, System.Generics.Collections;

{ TThreadSafeQueue

  PURPOUSE:
  Thread-safe implementation of first-in first-out queue.

  ENQUEUE:
  Non-blocking primitive. Stores an item into the queue, so it becomes
  non empty.

  DEQUEUE:
  Calling thread is locked while queue is empty, then an item is extracted
  from the queue and returned. At Instance destruction, ESyncObjectException
  is raised.

  CLEAR:
  Delete all items. Blocking primitive.

  EXAMPLE:

  while not Terminated do
  try
  item := q.dequeue(item)
  <<do something with item>>
  except
  on ESyncObjectException do Terminate;
  end;
}

type
  TThreadSafeQueue<T> = class
  private type
    TQueueNode = class
      Next: TQueueNode;
      Item: T;
    end;
  private
    FHead: TQueueNode;
    FTail: TQueueNode;
    CSTail: TCriticalSection;
    CSHead: TCriticalSection;
    NotEmpty: TPrecondition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Enqueue(Item: T);
    function Dequeue: T;
  end;

  { TThreadSafeStack

    PURPOUSE:
    Thread-safe implementation of last-in first-out stack.

    PUSH:
    Non-blocking primitive. Stores an item at the top of the queue,
    so it becomes non empty.

    POP:
    Calling thread is locked while stack is empty, then, top item is extracted
    from the stack and returned. At Instance destruction, ESyncObjectException
    is raised.

    EXAMPLE:

    while not Terminated do
    try
    item := stack.pop(item)
    <<do something with item>>
    except
    on ESyncObjectException do Terminate;
    end;
  }

type
  TThreadSafeStack<T> = class
  private
    FImpl: TStack<T>;
    CS: TCriticalSection;
    NotEmpty: TPrecondition;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(Item: T);
    function Pop: T;
  end;

implementation

uses
  sysutils;

// -----------------------------------------------------------------------------
// TThreadSafeQueue
// -----------------------------------------------------------------------------

constructor TThreadSafeQueue<T>.Create;
begin
  // closed := false;
  FHead := TQueueNode.Create;
  FHead.Next := nil;
  FTail := FHead;
  CSHead := TCriticalSection.Create;
  CSTail := TCriticalSection.Create;
  NotEmpty := TPrecondition.Create(
    function: boolean
    begin
      Result := (FHead.Next <> nil)
    end);
end;

// -----------------------------------------------------------------------------

destructor TThreadSafeQueue<T>.Destroy;
var
  oldHead: TQueueNode;
begin
  NotEmpty.Free; // may raise ESyncObjectException
  NotEmpty := nil;
  Clear;

  // free other objects
  CSHead.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeQueue<T>.Enqueue(Item: T);
var
  NewNode: TQueueNode;
begin
  NewNode := TQueueNode.Create;
  NewNode.Item := Item;
  NewNode.Next := nil;
  CSTail.Acquire;
  try
    FTail.Next := NewNode;
    FTail := NewNode;
  finally
    CSTail.Release;
  end;
  NotEmpty.Update;
end;

// -----------------------------------------------------------------------------

function TThreadSafeQueue<T>.Dequeue: T;
var
  OldNode: TQueueNode;
begin
  OldNode := nil;
  CSHead.Acquire;
  try
    if (NotEmpty.WaitFor(CSHead)) then
    begin
      OldNode := FHead;
      FHead := FHead.Next;
      Result := FHead.Item;
    end
  finally
    CSHead.Release;
  end;
  if (OldNode <> nil) then
  begin
    OldNode.Free;
    NotEmpty.Update;
  end;
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeQueue<T>.Clear;
var
  oldHead: TQueueNode;
begin
  CSHead.Acquire;
  try
    while (FHead.Next <> nil) do
    begin
      oldHead := FHead;
      FHead := FHead.Next;
      oldHead.Free;
    end;
  finally
    CSHead.Release;
  end;
  if (NotEmpty <> nil) then
  begin
    NotEmpty.Update;
  end; // else this object is being destroyed
end;

// -----------------------------------------------------------------------------
// TThreadSafeStack
// -----------------------------------------------------------------------------

constructor TThreadSafeStack<T>.Create;
begin
  CS := TCriticalSection.Create;
  FImpl := TStack<T>.Create;
  NotEmpty := TPrecondition.Create(
    function: boolean
    begin
      Result := (FImpl.Count > 0)
    end);
end;

// -----------------------------------------------------------------------------

destructor TThreadSafeStack<T>.Destroy;
begin
  NotEmpty.Free;
  FImpl.Free;
  CS.Free;
  inherited;
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeStack<T>.Push(Item: T);
begin
  CS.Acquire;
  try
    FImpl.Push(Item);
  finally
    CS.Release;
  end;
  NotEmpty.Update;
end;

// -----------------------------------------------------------------------------

function TThreadSafeStack<T>.Pop: T;
var
  wr: boolean;
begin
  wr := false;
  CS.Acquire;
  try
    wr := NotEmpty.WaitFor(CS);
    if (FImpl.Count > 0) then
      Result := FImpl.Pop
  finally
    CS.Release;
  end;
  if (wr) then
  begin
    NotEmpty.Update;
  end;
end;

// -----------------------------------------------------------------------------

procedure TThreadSafeStack<T>.Clear;
begin
  CS.Acquire;
  try
    FImpl.Clear;
  finally
    CS.Release;
  end;
  NotEmpty.Update;
end;

// -----------------------------------------------------------------------------

end.
