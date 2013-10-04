program Gallery;

{$R 'GalleryRes.res' 'Ribbon\GalleryRes.rc'}

uses
  Forms,
  FMain in 'FMain.pas' {FormMain},
  uRenderer in 'uRenderer.pas',
  RibbonConst in 'Ribbon\RibbonConst.pas',
  uShapeHandler in 'uShapeHandler.pas',
  uSizeAndColorHandler in 'uSizeAndColorHandler.pas',
  uBorderStyleHandler in 'uBorderStyleHandler.pas',
  uBorderSizeHandler in 'uBorderSizeHandler.pas',
  uLayoutHandler in 'uLayoutHandler.pas',
  uButtonHandler in 'uButtonHandler.pas',
  uPropertySet in 'uPropertySet.pas',
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
