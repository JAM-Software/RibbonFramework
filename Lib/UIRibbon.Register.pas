unit UIRibbon.Register;

interface

procedure Register;

implementation

uses
  Classes, UIRibbon, UIRibbonActions, System.Actions;

procedure Register;
begin
  RegisterComponents('Windows Ribbon Framework for Delphi', [TUIRibbon]);
  RegisterActions('Ribbon framewrok', [TRibbonCollectionAction], nil);
end;

end.
