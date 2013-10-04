program DropDownColorPicker;

{$R 'ribbonmarkup.res' 'Ribbon\ribbonmarkup.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  uRenderer in 'uRenderer.pas',
  uCommandHandler in 'uCommandHandler.pas',
  RibbonConsts in 'Ribbon\RibbonConsts.pas',
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
