unit FControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ImgList, ExtCtrls, RibbonMarkup;

type
  TFrameControl = class(TFrameCommandRefObject)
    LabelApplicationModes: TLabel;
    EditApplicationModes: TButtonedEdit;
    procedure EditApplicationModesRightButtonClick(Sender: TObject);
  strict private
    FControl: TRibbonControl;
    function ApplicationModesToString(const AppModes: Cardinal): String;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
    property Control: TRibbonControl read FControl;
  public
    { Public declarations }
  end;

resourcestring
  RS_ALL = '(all)';

implementation

uses
  DMShared,
  FApplicationModes;

{$R *.dfm}

{ TFrameControl }

function TFrameControl.ApplicationModesToString(
  const AppModes: Cardinal): String;
var
  I: Integer;
begin
  if (AppModes = $FFFFFFFF) then
    Result := RS_ALL
  else
  begin
    Result := '';
    for I := 0 to 31 do
    begin
      if ((AppModes and (1 shl I)) <> 0) then
      begin
        if (Result <> '') then
          Result := Result + ',';
        Result := Result + IntToStr(I);
      end;
    end;
  end;
end;

procedure TFrameControl.EditApplicationModesRightButtonClick(Sender: TObject);
var
  Form: TFormApplicationModes;
begin
  Form := TFormApplicationModes.Create(FControl.ApplicationModes);
  try
    if (Form.ShowModal = mrOk) then
    begin
      if (Form.AppModes <> FControl.ApplicationModes) then
      begin
        FControl.ApplicationModes := Form.AppModes;
        EditApplicationModes.Text := ApplicationModesToString(FControl.ApplicationModes);
        Modified;
      end;
    end;
  finally
    Form.Release;
  end;
end;

procedure TFrameControl.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FControl := Subject as TRibbonControl;
  EditApplicationModes.OnRightButtonClick := EditApplicationModesRightButtonClick;
  EditApplicationModes.Images := DataModuleShared.ImageListAppModes;
  LabelApplicationModes.Enabled := FControl.SupportApplicationModes;
  EditApplicationModes.Enabled := FControl.SupportApplicationModes;
  EditApplicationModes.RightButton.Enabled := FControl.SupportApplicationModes;
  if EditApplicationModes.Enabled then
    EditApplicationModes.Text := ApplicationModesToString(FControl.ApplicationModes)
  else
    EditApplicationModes.Text := '';
end;

end.
