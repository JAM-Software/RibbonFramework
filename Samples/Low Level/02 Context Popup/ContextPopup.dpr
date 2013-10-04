program ContextPopup;

{$R 'ContextPopupUI.res' 'Ribbon\ContextPopupUI.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  ContextPopupConst in 'Ribbon\ContextPopupConst.pas',
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
