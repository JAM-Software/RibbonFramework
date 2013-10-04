program SimpleRibbon;

{$R 'SimpleRibbonUI.res' 'Ribbon\SimpleRibbonUI.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  UIRibbonApi in '..\..\..\Lib\UIRibbonApi.pas',
  WinApiEx in '..\..\..\Lib\WinApiEx.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
