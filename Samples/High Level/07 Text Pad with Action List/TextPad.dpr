program TextPad;

{$R 'Galleries.res' 'Galleries.rc'}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  FMain in 'FMain.pas' {FormMain},
  RibbonMarkup in 'Ribbon\RibbonMarkup.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
