program TextPad;

{$R 'Galleries.res' 'Galleries.rc'}
{$R 'UIRibbon.res' '..\..\..\Lib\UIRibbon.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  UIRibbon in '..\..\..\Lib\UIRibbon.pas',
  UIRibbonApi in '..\..\..\Lib\UIRibbonApi.pas',
  UIRibbonCommands in '..\..\..\Lib\UIRibbonCommands.pas',
  UIRibbonForm in '..\..\..\Lib\UIRibbonForm.pas',
  UIRibbonUtils in '..\..\..\Lib\UIRibbonUtils.pas',
  WinApiEx in '..\..\..\Lib\WinApiEx.pas',
  RibbonMarkup in 'Ribbon\RibbonMarkup.pas',
  RichEditEx in 'RichEditEx.pas',
  UIRibbonActions in '..\..\..\Lib\UIRibbonActions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
