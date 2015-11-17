unit UIRibbon.Register;

interface

procedure Register;

implementation

uses
  Classes, UIRibbon;

procedure Register;
begin
  RegisterComponents('Windows Ribbon Framework for Delphi', [TUIRibbon]);
end;

end.
