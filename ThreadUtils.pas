{ *******************************************************

  Threading utilities for Delphi 10 and above

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

unit ThreadUtils;

{
  SUMMARY:

  - TCustomThread<ArgType>: simplify the writing of thread code,
  avoiding the need to derive a new class.

  - TTaskHelper: additional functionality to TTask. Avoid usage of IFuture.
}

interface

uses
  System.Classes, System.SysUtils, System.Threading;

{ TCustomThread<ArgType>

  PURPOUSE:
  To simplify the writing of thread code, avoiding the need to derive a new
  class. Thread code is written as an event handler or as a reference to
  procedure. Custom data of ArgType will be passed at thread initialization.

  GENERAL USAGE:
  - Write an event handler (TNotifyEvent) or a reference to procedure.
  - Create a TCustomThread instance. Custom data may be passed to the newly
  created thread, which may be accessed later thanks to the CustomData
  property.
  - Use it as any other TThread instance.

  THREAD PROPERTIES:
  Protected TThread properties and methods are made public so you can check
  for thread termination or call the synchronize method.

  EVENT HANDLER, example:
  The argument to TNotifyEvent should be typecasted to TCustomThread<ArgType>.

  procedure TMyClass.ThreadBody(Sender: TObject);
  begin
  with Sender as TCustomThread<integer> do
  while (not Terminated) do
  begin
  ...

  procedure TMyClass.Other;
  begin
  thread := TCustomThread<integer>.Create(true,1,ThreadBody);
  ...

  REFERENCE TO PROCEDURE, example:

  thread := TCustomThread<integer>.Create(
  true,1,
  procedure(instance: TCustomThread<integer>)
  begin
  with instance do
  while (not terminated) do
  begin
  ...
  end)
  ...
}

type
  TCustomThread<ArgType> = class(TThread)
  private
    FOnExecute1: TNotifyEvent;
    FOnExecute2: TProc<TCustomThread<ArgType>>;
    FCustomData: ArgType;
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: boolean; InitialData: ArgType;
      OnExecute: TNotifyEvent); overload;
    constructor Create(CreateSuspended: boolean; InitialData: ArgType;
      OnExecute: TProc < TCustomThread < ArgType >> ); overload;

    procedure Synchronize(AMethod: TThreadMethod); overload;
    procedure Synchronize(AThreadProc: TThreadProcedure); overload;

    property CustomData: ArgType read FCustomData;

    property Terminated;
  end;

  TCustomThread = TCustomThread<TObject>;

  {
    TTaskHelper

    PURPOUSE:
    Add a simplified TTask.Run method which can run both procedures
    and functions. No need to use IFuture on event-driven apps.

    USAGE:
    Call Run to execute some code in another thread.
    Provide an "OnTerminate" event handler to be called after the task
    is finished. This WILL BE CALLED AT THE MAIN THREAD.
    If the given task raised an exception, "RaisedException" will be bound to it,
    nil otherwise.

    EXAMPLE:

    TTask.Run<integer>(
    function: integer
    begin
    Result := ...(some value)
    end),onTaskTerminate)

    procedure TForm1.onTaskTerminate(Sender: TObject; E:
    Exception; value: integer);
    begin
    if (E <> nil) then
    ...(do something with "E")...
    else
    ...(do something with "value") ...
    end;
  }
type
  TTaskHelper = class helper for TTask
  public type
    TFuncNotifyEvent<T> = procedure(Sender: TObject; RaisedException: Exception;
      TaskResult: T) of object;
    TProcNotifyEvent = procedure(Sender: TObject; RaisedException: Exception)
      of object;
  public
    class function Run<T>(const Func: TFunc<T>;
      OnTerminate: TTaskHelper.TFuncNotifyEvent<T>; Sender: TObject = nil)
      : ITask; overload; static;
    class function Run(const Func: TProc;
      OnTerminate: TTaskHelper.TProcNotifyEvent; Sender: TObject = nil): ITask;
      overload; static;
  end;

  // ---------------------------------------------------------------------------

implementation

// -----------------------------------------------------------------------------
// TCustomThread
// -----------------------------------------------------------------------------

constructor TCustomThread<ArgType>.Create(CreateSuspended: boolean;
  InitialData: ArgType; OnExecute: TNotifyEvent);
begin
  FOnExecute1 := OnExecute;
  FOnExecute2 := nil;
  FCustomData := InitialData;
  inherited Create(CreateSuspended);
end;

// -----------------------------------------------------------------------------

constructor TCustomThread<ArgType>.Create(CreateSuspended: boolean;
  InitialData: ArgType; OnExecute: TProc < TCustomThread < ArgType >> );
begin
  FOnExecute1 := nil;
  FOnExecute2 := OnExecute;
  FCustomData := InitialData;
  inherited Create(CreateSuspended);
end;

// -----------------------------------------------------------------------------

procedure TCustomThread<ArgType>.Execute;
begin
  if (Assigned(FOnExecute1)) then
    FOnExecute1(self)
  else if (Assigned(FOnExecute2)) then
    FOnExecute2(self);
end;

// -----------------------------------------------------------------------------

procedure TCustomThread<ArgType>.Synchronize(AMethod: TThreadMethod);
begin
  inherited Synchronize(AMethod);
end;

// -----------------------------------------------------------------------------

procedure TCustomThread<ArgType>.Synchronize(AThreadProc: TThreadProcedure);
begin
  inherited Synchronize(AThreadProc);
end;

// -----------------------------------------------------------------------------
// TTaskHelper
// -----------------------------------------------------------------------------

class function TTaskHelper.Run<T>(const Func: TFunc<T>;
  OnTerminate: TTaskHelper.TFuncNotifyEvent<T>; Sender: TObject = nil): ITask;
var
  dataToReturn: T;
  raisedE: Exception;
begin
  if (Assigned(Func)) and (Assigned(OnTerminate)) then
  begin
    Result := TTask.Run(
      procedure
      begin
        try
          raisedE := nil;
          dataToReturn := Func;
        except
          raisedE := Exception(AcquireExceptionObject);
        end;
        TThread.Synchronize(nil,
          procedure
          begin
            OnTerminate(Sender, raisedE, dataToReturn);
            if (raisedE <> nil) then
              ReleaseExceptionObject;
          end);
      end);
  end
  else
    Result := nil;
end;

// -----------------------------------------------------------------------------

class function TTaskHelper.Run(const Func: TProc;
OnTerminate: TTaskHelper.TProcNotifyEvent; Sender: TObject = nil): ITask;
var
  raisedE: Exception;
begin
  if (Assigned(Func)) and (Assigned(OnTerminate)) then
  begin
    Result := TTask.Run(
      procedure
      begin
        try
          raisedE := nil;
          Func;
        except
          raisedE := Exception(AcquireExceptionObject);
        end;
        TThread.Synchronize(TThread.CurrentThread,
          procedure
          begin
            OnTerminate(Sender, raisedE);
            if (raisedE <> nil) then
              ReleaseExceptionObject;
          end);
      end);
  end
  else
    Result := nil;
end;

// -----------------------------------------------------------------------------

end.
