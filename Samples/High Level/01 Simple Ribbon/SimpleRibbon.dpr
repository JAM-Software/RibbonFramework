program SimpleRibbon;

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  UIRibbon in '..\..\..\Lib\UIRibbon.pas',
  UIRibbonForm in '..\..\..\Lib\UIRibbonForm.pas',
  UIRibbonUtils in '..\..\..\Lib\UIRibbonUtils.pas',
  UIRibbonApi in '..\..\..\Lib\UIRibbonApi.pas',
  UIRibbonCommands in '..\..\..\Lib\UIRibbonCommands.pas',
  WinApiEx in '..\..\..\Lib\WinApiEx.pas',
  SimpleRibbonUI in 'Ribbon\SimpleRibbonUI.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
