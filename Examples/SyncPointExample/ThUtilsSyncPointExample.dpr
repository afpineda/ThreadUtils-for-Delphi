program ThUtilsSyncPointExample;

uses
  Vcl.Forms,
  ThUtilsSyncPointExampleMain in 'ThUtilsSyncPointExampleMain.pas' {Form_main},
  ThreadUtils in '..\..\ThreadUtils.pas',
  ThreadUtils.Sync in '..\..\ThreadUtils.Sync.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm_main, Form_main);
  Application.Run;
end.
