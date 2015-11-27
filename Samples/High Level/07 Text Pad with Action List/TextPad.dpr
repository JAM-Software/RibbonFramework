program TextPad;

{$R 'Galleries.res' 'Galleries.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  RibbonMarkup in 'Ribbon\RibbonMarkup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
