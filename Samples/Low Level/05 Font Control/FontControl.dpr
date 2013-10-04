program FontControl;



{$R 'RibbonMarkup.res' 'Ribbon\RibbonMarkup.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  UIRibbonApi in '..\..\..\Lib\UIRibbonApi.pas',
  WinApiEx in '..\..\..\Lib\WinApiEx.pas',
  uRichEditManager in 'uRichEditManager.pas',
  uPropertyStore in 'uPropertyStore.pas',
  RibbonConst in 'Ribbon\RibbonConst.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
