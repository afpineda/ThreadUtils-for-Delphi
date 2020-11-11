program ThUtilsQueueExample;

uses
  Vcl.Forms,
  ThUtilsQueueExampleMain in 'ThUtilsQueueExampleMain.pas' {Form_main},
  ThreadUtils in '..\..\ThreadUtils.pas',
  ThreadUtils.SafeDataTypes in '..\..\ThreadUtils.SafeDataTypes.pas',
  ThreadUtils.Sync in '..\..\ThreadUtils.Sync.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
