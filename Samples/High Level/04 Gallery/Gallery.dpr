program Gallery;

{$R 'GalleryRes.res'}
uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  uRenderer in 'uRenderer.pas',
  RibbonConst in 'Ribbon\RibbonConst.pas',
  UIRibbonApi in '..\..\..\Lib\UIRibbonApi.pas',
  UIRibbon in '..\..\..\Lib\UIRibbon.pas',
  UIRibbonCommands in '..\..\..\Lib\UIRibbonCommands.pas',
  UIRibbonForm in '..\..\..\Lib\UIRibbonForm.pas',
  UIRibbonUtils in '..\..\..\Lib\UIRibbonUtils.pas',
  WinApiEx in '..\..\..\Lib\WinApiEx.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
