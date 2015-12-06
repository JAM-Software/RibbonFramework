unit UIRibbon.Register;

interface

procedure Register;

implementation

uses
  Classes, UIRibbon, UIRibbonActions, System.Actions;

procedure Register;
begin
  RegisterComponents('Windows Ribbon Framework for Delphi', [TUIRibbon]);
  RegisterActions('Ribbon Framework', [TRibbonCollectionAction, TRibbonFontAction, TRibbonColorAction], nil);
end;

end.
