unit ThUtilsSyncPointExampleMain;

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

  *******************************************************

  This example shows:
  - ThreadUtils.Sync.TSyncPoint
  - ThreadUtils.TTaskHelper (at implementation section)
  - ThreadUtils.Sync.TThreadSyncHelper (at implementation section)

  This trivial example shows two different ways for waiting
  for a bunch of threads to terminate. However, TSyncPoint
  works best whenever you need an "OnDestroy" handler
  (not in this example).

  ******************************************************* }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  ThreadUtils.Sync;

const
  WORKERS_COUNT = 10;

type
  TForm_main = class(TForm)
    Panel1: TPanel;
    Btn_stopBySyncPoint: TButton;
    Memo_log: TMemo;
    Btn_stopByWaitFor: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Btn_stopBySyncPointClick(Sender: TObject);
    procedure Btn_stopByWaitForClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    stopPlease: Boolean; // ask workers to stop
    EndOfWork: TSyncPoint; // signal end of work
    WorkerArray: array [1 .. WORKERS_COUNT] of TThread; // keep track of workers
    procedure OnWork(Sender: TObject); // Some stupid thread work
    procedure OnWorkEnd(Sender: TObject; E: Exception); // Event: end of work
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  ThreadUtils, System.Threading;

{$R *.dfm}

// work of a busy thread
procedure TForm_main.OnWork(Sender: TObject);
var
  busyTimeMs: integer;
begin
  with Sender as TCustomThread<integer> do
    while (Not stopPlease) do
    begin
      busyTimeMs := Random(10000);
      sleep(busyTimeMs);
      Synchronize(
        procedure
        begin
          Memo_log.Lines.Add('Worker ' + IntToStr(CustomData) + ' was here!');
        end);
    end;
  EndOfWork.Reached;
end;

// Ask workers to stop by the means of "EndOfWork" syncpoint

procedure TForm_main.Btn_stopBySyncPointClick(Sender: TObject);
begin
  Btn_stopBySyncPoint.Enabled := false;
  Memo_log.Lines.Add('Please, stop now');
  // This background task will wait for end of work then execute "OnWorkEnd"
  // at the main thread
  TTask.Run(
    procedure
    begin
      // all threads must execute "EndOfWork.Reached" for this primitive
      // to unlock
      EndOfWork.WaitFor();
    end, OnWorkEnd); // Note: this is a helper method
  stopPlease := true;
end;

// Ask workers to stop by the means of "WaitForAll" helper method

procedure TForm_main.Btn_stopByWaitForClick(Sender: TObject);
begin
  Btn_stopByWaitFor.Enabled := false;
  Memo_log.Lines.Add('You shall not pass!!!');
  // This background task will wait for end of work then execute "OnWorkEnd"
  // at the main thread
  TTask.Run(
    procedure
    begin
      // all threads must execute "EndOfWork.Reached" for this primitive
      // to unlock
      TThread.WaitForAll(WorkerArray); // Note: this is a helper method
    end, OnWorkEnd); // Note: this is a helper method
  stopPlease := true;
end;

// Event: all workers finished

procedure TForm_main.OnWorkEnd(Sender: TObject; E: Exception);
begin
  Memo_log.Lines.Add('All workers are gone, never to return');
  stopPlease := false; // allows app window to be closed
end;

// App can not be closed while waiting for workers to terminate

procedure TForm_main.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not stopPlease;
end;

// App initialization

procedure TForm_main.FormCreate(Sender: TObject);
var
  I: integer;
begin
  // GUI initialization
  Memo_log.Clear;

  // Create sync point
  EndOfWork := TSyncPoint.Create(WORKERS_COUNT);

  // Start workers
  // WorkerArray is required to enable a further call to WaitForAll
  stopPlease := false;
  for I := 1 to WORKERS_COUNT do
  begin
    WorkerArray[I] := TCustomThread<integer>.Create(false, I, OnWork);
  end;
end;

end.
