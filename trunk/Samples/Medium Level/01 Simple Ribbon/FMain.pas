unit FMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UIRibbonForm;

type
  TFormMain = class(TUIRibbonForm)
  private
    { Private declarations }
  public
    { Public declarations }
    class function RibbonResourceName: String; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ TFormMain }

class function TFormMain.RibbonResourceName: String;
begin
  Result := 'SIMPLERIBBON';
end;

end.
