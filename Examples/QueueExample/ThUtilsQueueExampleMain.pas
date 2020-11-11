unit ThUtilsQueueExampleMain;

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

  *******************************************************

  This example shows:
  - ThreadUtils.SafeDataTypes.TThreadSafeQueue
  - ThreadUtils.Sync.TSyncCounter
  - ThreadUtils.TCustomThread (at implementation section)

  ******************************************************* }
interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ThreadUtils.SafeDataTypes, Vcl.StdCtrls,
  Vcl.ExtCtrls, System.Threading, ThreadUtils.Sync;

type
  TForm_main = class(TForm)
    Panel1: TPanel;
    Memo_enqueue: TMemo;
    Memo_dequeue2: TMemo;
    Memo_dequeue1: TMemo;
    Btn_Enqueue: TButton;
    Btn_Cancel: TButton;
    Lbl_status: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Btn_EnqueueClick(Sender: TObject);
    procedure Btn_CancelClick(Sender: TObject);
  private
    { Private declarations }
    SafeQueue: TThreadSafeQueue<integer>; // Our queue
    Idle: TSyncCounter; // Detect idle/busy workers
    procedure OnIdle(Sender: TObject); // event: workers become idle
    procedure OnBusy(Sender: TObject); // event: workers become busy
    procedure OnWork(Sender: TObject); // Simulated work on queued items
  public
    { Public declarations }
  end;

var
  Form_main: TForm_main;

implementation

uses
  System.SyncObjs,
  ThreadUtils;

{$R *.dfm}
// -----------------------------------------------------------------------------

// Form initialization

procedure TForm_main.FormCreate(Sender: TObject);
begin
  // Create thread-safe queue and idle/busy counter
  SafeQueue := TThreadSafeQueue<integer>.Create;
  Idle := TSyncCounter.Create;
  Idle.OnTargetReached := OnIdle;
  Idle.OnLeaveTarget := OnBusy;

  // Initialize GUI
  Memo_enqueue.Lines.Clear;
  Memo_enqueue.Lines.Add('Enqueued items:');
  Memo_dequeue1.Lines.Clear;
  Memo_dequeue1.Lines.Add('Worker 1:');
  Memo_dequeue2.Lines.Clear;
  Memo_dequeue2.Lines.Add('Worker 2:');

  // Two worker threads are spawn
  Idle.Initialize(2, 0);
  TCustomThread<integer>.Create(false,1,OnWork);
  TCustomThread<integer>.Create(false,2,OnWork);
end;

// Graceful app termination

procedure TForm_main.FormDestroy(Sender: TObject);
begin
  // Workers will terminate at SafeQueue destruction
  SafeQueue.Free;
  Idle.Free;
end;

// Clear all pending items at our queue

procedure TForm_main.Btn_CancelClick(Sender: TObject);
begin
  SafeQueue.Clear;
  Memo_enqueue.Lines.Add('All pending items removed');
end;

// Add a random item to our queue

procedure TForm_main.Btn_EnqueueClick(Sender: TObject);
var
  item: integer;
begin
  item := Random(65535);
  Memo_enqueue.Lines.Add(IntToStr(item));
  SafeQueue.Enqueue(item);
end;

// Simulated work on items

procedure TForm_main.OnWork(Sender: TObject);
var
  computeTimeMs: integer;
  myMemo: TMemo;
  item: integer;
begin
  with (Sender as TCustomThread<integer>) do
  begin
    // GUI stuff
    // Note: no need to syncronize access to each memo
    if (CustomData = 1) then
      myMemo := Memo_dequeue1
    else
      myMemo := Memo_dequeue2;

    // Infinite loop of work
    while (Not Terminated) do
      try
        // Set idle/bussy state and dequeue an item
        Idle.Inc;
        item := SafeQueue.Dequeue;
        Idle.Dec;
        myMemo.Lines.Add(IntToStr(item) + ' dequeued');

        // simulate work on dequeued item that takes some time
        computeTimeMs := Random(10000);
        sleep(computeTimeMs);
        myMemo.Lines.Add(IntToStr(item) + ' processed');
      except
        // An exception will be raised at SafeQueue destruction.
        // So, this loop is interrupted, then current thread gracefully
        // terminates
        On ESyncObjectException do Terminate;
      end;
  end;
end;

// Event: all workers are idle

procedure TForm_main.OnIdle(Sender: TObject);
begin
  Lbl_status.Caption := 'Idle';
end;

// Event: at least one worker is busy

procedure TForm_main.OnBusy(Sender: TObject);
begin
  Lbl_status.Caption := 'Working';
end;

end.
