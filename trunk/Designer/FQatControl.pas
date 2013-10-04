unit FQatControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, RibbonMarkup, pngimage;

type
  TFrameQatControl = class(TFrameCommandRefObject)
    CheckBoxIsChecked: TCheckBox;
    procedure CheckBoxIsCheckedClick(Sender: TObject);
  private
    { Private declarations }
    FQatControl: TRibbonQatControl;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameQatControl }

procedure TFrameQatControl.CheckBoxIsCheckedClick(Sender: TObject);
begin
  if (CheckBoxIsChecked.Checked <> FQatControl.IsChecked) then
  begin
    FQatControl.IsChecked := CheckBoxIsChecked.Checked;
    Modified;
  end;
end;

procedure TFrameQatControl.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FQatControl := Subject as TRibbonQatControl;
  CheckBoxIsChecked.Checked := FQatControl.IsChecked;
end;

end.
