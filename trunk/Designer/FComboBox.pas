unit FComboBox;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, StdCtrls, ExtCtrls, RibbonMarkup, pngimage;

type
  TFrameComboBox = class(TFrameCommandRefObject)
    CheckBoxEditable: TCheckBox;
    CheckBoxResizeable: TCheckBox;
    CheckBoxAutoComplete: TCheckBox;
    procedure CheckBoxEditableClick(Sender: TObject);
    procedure CheckBoxResizeableClick(Sender: TObject);
    procedure CheckBoxAutoCompleteClick(Sender: TObject);
  private
    { Private declarations }
    FComboBox: TRibbonComboBox;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameComboBox }

procedure TFrameComboBox.CheckBoxAutoCompleteClick(Sender: TObject);
begin
  if (CheckBoxAutoComplete.Checked <> FComboBox.IsAutoCompleteEnabled) then
  begin
    FComboBox.IsAutoCompleteEnabled := CheckBoxAutoComplete.Checked;
    Modified;
  end;
end;

procedure TFrameComboBox.CheckBoxEditableClick(Sender: TObject);
begin
  if (CheckBoxEditable.Checked <> FComboBox.IsEditable) then
  begin
    FComboBox.IsEditable := CheckBoxEditable.Checked;
    Modified;
  end;
end;

procedure TFrameComboBox.CheckBoxResizeableClick(Sender: TObject);
begin
  if (CheckBoxResizeable.Checked <> (FComboBox.ResizeType = rtVerticalResize)) then
  begin
    if (CheckBoxResizeable.Checked) then
      FComboBox.ResizeType := rtVerticalResize
    else
      FComboBox.ResizeType := rtNoResize;
    Modified;
  end;
end;

procedure TFrameComboBox.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FComboBox := Subject as TRibbonComboBox;
  CheckBoxEditable.Checked := FComboBox.IsEditable;
  CheckBoxResizeable.Checked := (FComboBox.ResizeType = rtVerticalResize);
  CheckBoxAutoComplete.Checked := FComboBox.IsAutoCompleteEnabled;
end;

end.
