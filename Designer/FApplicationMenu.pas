unit FApplicationMenu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FCommandRefObject, ImgList, StdCtrls, ExtCtrls, pngimage, RibbonMarkup,
  ComCtrls, FControl;

type
  TFrameApplicationMenu = class(TFrameCommandRefObject)
    GroupBoxRecentItems: TGroupBox;
    CheckBoxEnableRecentItems: TCheckBox;
    LabelMaxCount: TLabel;
    EditMaxCount: TEdit;
    UpDownMaxCount: TUpDown;
    CheckBoxEnablePinning: TCheckBox;
    LabelCaptionCommand: TLabel;
    ComboBoxCaptionCommand: TComboBox;
    procedure CheckBoxEnableRecentItemsClick(Sender: TObject);
    procedure EditMaxCountChange(Sender: TObject);
    procedure CheckBoxEnablePinningClick(Sender: TObject);
    procedure ComboBoxCaptionCommandChange(Sender: TObject);
  private
    { Private declarations }
    FAppMenu: TRibbonApplicationMenu;
    procedure UpdateControls;
  strict protected
    procedure Initialize(const Subject: TRibbonObject); override;
  public
    { Public declarations }
    procedure Activate; override;
  end;

implementation

{$R *.dfm}

uses
  FViews;

{ TFrameApplicationMenu }

procedure TFrameApplicationMenu.Activate;
var
  FrameViews: TFrameViews;
  CurrentCmd: String;
begin
  inherited;
  FrameViews := Owner as TFrameViews;
  CurrentCmd := ComboBoxCaptionCommand.Text;
  ComboBoxCaptionCommand.Items := FrameViews.Commands;
  if (CurrentCmd = '') then
    ComboBoxCaptionCommand.ItemIndex := 0
  else
  begin
    ComboBoxCaptionCommand.ItemIndex := ComboBoxCaptionCommand.Items.IndexOf(CurrentCmd);
    if (ComboBoxCaptionCommand.ItemIndex < 0) then
      ComboBoxCaptionCommand.ItemIndex := 0;
  end;
end;

procedure TFrameApplicationMenu.CheckBoxEnablePinningClick(Sender: TObject);
begin
  inherited;
  if Assigned(FAppMenu.RecentItems) and (CheckBoxEnablePinning.Checked <> FAppMenu.RecentItems.EnablePinning) then
  begin
    FAppMenu.RecentItems.EnablePinning := CheckBoxEnablePinning.Checked;
    Modified;
  end;
end;

procedure TFrameApplicationMenu.CheckBoxEnableRecentItemsClick(Sender: TObject);
begin
  inherited;
  FAppMenu.EnableRecentItems(CheckBoxEnableRecentItems.Checked);
  UpdateControls;
  Modified;
end;

procedure TFrameApplicationMenu.ComboBoxCaptionCommandChange(Sender: TObject);
var
  NewRef: TRibbonCommand;
begin
  if Assigned(FAppMenu.RecentItems) then
  begin
    if (ComboBoxCaptionCommand.ItemIndex < 0) then
      NewRef := nil
    else
      NewRef := ComboBoxCaptionCommand.Items.Objects[ComboBoxCaptionCommand.ItemIndex] as TRibbonCommand;
    if (NewRef <> FAppMenu.RecentItems.CommandRef) then
    begin
      FAppMenu.RecentItems.CommandRef := NewRef;
      Modified;
    end;
  end;
end;

procedure TFrameApplicationMenu.EditMaxCountChange(Sender: TObject);
begin
  inherited;
  if Assigned(FAppMenu.RecentItems) and (UpDownMaxCount.Position <> FAppMenu.RecentItems.MaxCount) then
  begin
    FAppMenu.RecentItems.MaxCount := UpDownMaxCount.Position;
    Modified;
  end;
end;

procedure TFrameApplicationMenu.Initialize(const Subject: TRibbonObject);
begin
  inherited;
  FAppMenu := Subject as TRibbonApplicationMenu;
  CheckBoxEnableRecentItems.Checked := Assigned(FAppMenu.RecentItems);
  UpdateControls;
end;

procedure TFrameApplicationMenu.UpdateControls;
begin
  LabelCaptionCommand.Enabled := Assigned(FAppMenu.RecentItems);
  ComboBoxCaptionCommand.Enabled := Assigned(FAppMenu.RecentItems);
  LabelMaxCount.Enabled := Assigned(FAppMenu.RecentItems);
  EditMaxCount.Enabled := Assigned(FAppMenu.RecentItems);
  UpDownMaxCount.Enabled := Assigned(FAppMenu.RecentItems);
  CheckBoxEnablePinning.Enabled := Assigned(FAppMenu.RecentItems);

  if Assigned(FAppMenu.RecentItems) then
  begin
    if (FAppMenu.RecentItems.CommandRef = nil) then
      ComboBoxCaptionCommand.ItemIndex := 0
    else
      ComboBoxCaptionCommand.ItemIndex := ComboBoxCaptionCommand.Items.IndexOfObject(FAppMenu.RecentItems.CommandRef);
    UpDownMaxCount.Position := FAppMenu.RecentItems.MaxCount;
    CheckBoxEnablePinning.Checked := FAppMenu.RecentItems.EnablePinning;
  end
  else
  begin
    ComboBoxCaptionCommand.ItemIndex := -1;
    UpDownMaxCount.Position := 0;
    CheckBoxEnablePinning.Checked := False;
  end;
end;

end.
