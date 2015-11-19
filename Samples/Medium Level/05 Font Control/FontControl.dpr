program FontControl;

{$R 'Ribbon\ribbonmarkup.RES'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  UIRibbonApi in '..\..\..\Lib\UIRibbonApi.pas',
  WinApiEx in '..\..\..\Lib\WinApiEx.pas',
  uRichEditManager in 'uRichEditManager.pas',
  RibbonConst in 'Ribbon\RibbonConst.pas',
  UIRibbon in '..\..\..\Lib\UIRibbon.pas',
  UIRibbonCommands in '..\..\..\Lib\UIRibbonCommands.pas',
  UIRibbonForm in '..\..\..\Lib\UIRibbonForm.pas',
  UIRibbonUtils in '..\..\..\Lib\UIRibbonUtils.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
