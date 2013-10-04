unit FAppMenuGroup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, pngimage, ExtCtrls, RibbonMarkup;

type
  TFrameAppMenuGroup = class(TFrameCommandRefObject)
  private
    { Private declarations }
  public
    { Public declarations }
    FMenuGroup: TRibbonAppMenuGroup;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  end;

implementation

{$R *.dfm}

{ TFrameAppMenuGroup }

procedure TFrameAppMenuGroup.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FMenuGroup := Subject as TRibbonAppMenuGroup;
end;

end.
