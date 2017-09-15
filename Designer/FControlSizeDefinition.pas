unit FControlSizeDefinition;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FBaseFrame, ExtCtrls, StdCtrls, RibbonMarkup;

type
  TFrameControlSizeDefintion = class(TBaseFrame)
    LabelHeader: TLabel;
    Label1: TLabel;
    ComboBoxControlName: TComboBox;
    Label2: TLabel;
    ComboBoxImageSize: TComboBox;
    CheckBoxIsLabelVisible: TCheckBox;
    CheckBoxIsImageVisible: TCheckBox;
    CheckBoxIsPopup: TCheckBox;
    procedure ComboBoxControlNameChange(Sender: TObject);
    procedure ComboBoxImageSizeChange(Sender: TObject);
    procedure CheckBoxIsLabelVisibleClick(Sender: TObject);
    procedure CheckBoxIsImageVisibleClick(Sender: TObject);
    procedure CheckBoxIsPopupClick(Sender: TObject);
  private
    { Private declarations }
    FSizeDef: TRibbonControlSizeDefinition;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrameControlSizeDefintion }

procedure TFrameControlSizeDefintion.CheckBoxIsImageVisibleClick(
  Sender: TObject);
begin
  if (CheckBoxIsImageVisible.Checked <> FSizeDef.IsImageVisible) then
  begin
    FSizeDef.IsImageVisible := CheckBoxIsImageVisible.Checked;
    Modified;
  end;
end;

procedure TFrameControlSizeDefintion.CheckBoxIsLabelVisibleClick(
  Sender: TObject);
begin
  if (CheckBoxIsLabelVisible.Checked <> FSizeDef.IsLabelVisible) then
  begin
    FSizeDef.IsLabelVisible := CheckBoxIsLabelVisible.Checked;
    Modified;
  end;
end;

procedure TFrameControlSizeDefintion.CheckBoxIsPopupClick(Sender: TObject);
begin
  if (CheckBoxIsPopup.Checked <> FSizeDef.IsPopup) then
  begin
    FSizeDef.IsPopup := CheckBoxIsPopup.Checked;
    Modified;
  end;
end;

procedure TFrameControlSizeDefintion.ComboBoxControlNameChange(Sender: TObject);
var
  ControlName: String;
begin
  ControlName := ComboBoxControlName.Text;
  if (ControlName <> FSizeDef.ControlName) then
  begin
    FSizeDef.ControlName := ControlName;
    Modified;
  end;
end;

procedure TFrameControlSizeDefintion.ComboBoxImageSizeChange(Sender: TObject);
begin
  if (ComboBoxImageSize.ItemIndex <> Ord(FSizeDef.ImageSize)) then
  begin
    FSizeDef.ImageSize := TRibbonImageSize(ComboBoxImageSize.ItemIndex);
    Modified;
  end;
end;

procedure TFrameControlSizeDefintion.Initialize(const Subject: TRibbonObject);
var
  ControlName: String;
begin
  inherited;
  FSizeDef := Subject as TRibbonControlSizeDefinition;
  ComboBoxControlName.Items.Clear;
  for ControlName in FSizeDef.OwnerDefinition.ControlNameMap.ControlNameDefinitions do
    ComboBoxControlName.Items.Add(ControlName);
  ComboBoxControlName.ItemIndex := ComboBoxControlName.Items.IndexOf(FSizeDef.ControlName);
  ComboBoxImageSize.ItemIndex := Ord(FSizeDef.ImageSize);
  CheckBoxIsLabelVisible.Checked := FSizeDef.IsLabelVisible;
  CheckBoxIsImageVisible.Checked := FSizeDef.IsImageVisible;
  CheckBoxIsPopup.Checked := FSizeDef.IsPopup;
end;

end.
